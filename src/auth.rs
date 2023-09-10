use crate::env_var_url;
use oauth2::basic::{BasicClient, BasicTokenResponse};
use oauth2::{
    AuthUrl, AuthorizationCode, ClientId, ClientSecret, CsrfToken, PkceCodeChallenge,
    PkceCodeVerifier, RedirectUrl, Scope, TokenUrl,
};
use worker::kv::KvStore;
use worker::*;

fn load_auth_store(env: &Env) -> Result<KvStore> {
    env.kv("AUTH_KV")
}

/// Creates an OAuth2 FIB authorization API client.
fn create_oauth_client(env: &Env) -> Result<BasicClient> {
    let client_id = env.secret("CLIENT_ID")?;
    let client_secret = env.secret("CLIENT_SECRET")?;

    let api_base = env_var_url(env, "FIB_API_BASE")?;
    let auth_url = api_base.join("o/authorize")?;
    let token_url = api_base.join("o/token")?;

    let client = BasicClient::new(
        ClientId::new(client_id.to_string()),
        Some(ClientSecret::new(client_secret.to_string())),
        AuthUrl::from_url(auth_url),
        Some(TokenUrl::from_url(token_url)),
    );

    // Set the URL the user will be redirected to after the auth process.
    let app_base = env_var_url(env, "APP_BASE")?;
    let redirect_url = app_base.join("export")?;
    Ok(client.set_redirect_uri(RedirectUrl::from_url(redirect_url)))
}

/// The data used to validate a response from the FIB API
/// to an authorization requested.
struct AuthResponseVerifier {
    csrf_token: CsrfToken,
    pkce_verifier: PkceCodeVerifier,
}

/// Saves the validation data for a future response to an authorization request.
async fn save_request(verifier: AuthResponseVerifier, env: &Env) -> Result<()> {
    const ONE_DAY: u64 = 60 * 60 * 24; // s
    let kv = load_auth_store(env)?;
    kv.put(
        verifier.csrf_token.secret(),
        verifier.pkce_verifier.secret(),
    )?
    .expiration_ttl(ONE_DAY)
    .execute()
    .await
    .map_err(Into::into)
}

pub async fn get_access_token(
    auth_code: AuthorizationCode,
    csrf_token: CsrfToken,
    env: &Env,
) -> Result<Option<BasicTokenResponse>> {
    let kv = load_auth_store(env)?;
    let auth_key = csrf_token.secret();
    let pkce_verifier = match kv.get(auth_key).text().await? {
        Some(v) => PkceCodeVerifier::new(v),
        // There's no pending auth request with the given CSRF token.
        None => return Ok(None),
    };

    let client = create_oauth_client(env)?;
    let token_res = client
        .exchange_code(auth_code)
        .set_pkce_verifier(pkce_verifier)
        .request_async(oauth2::reqwest::async_http_client)
        .await;
    match token_res {
        Ok(resp) => Ok(Some(resp)),
        Err(e) => {
            // todo: distinguish between user disagreement and graver errors.
            eprintln!("{e}");
            Ok(None)
        }
    }
}

pub(crate) async fn handle_auth(env: Env) -> Result<Response> {
    // Generate a PKCE challenge.
    let (pkce_challenge, pkce_verifier) = PkceCodeChallenge::new_random_sha256();

    // Generate the full authorization URL.
    let client = create_oauth_client(&env)?;
    let (auth_url, csrf_token) = client
        .authorize_url(CsrfToken::new_random)
        // Set the desired scopes.
        .add_scope(Scope::new("read".to_string()))
        // Set the PKCE code challenge.
        .set_pkce_challenge(pkce_challenge)
        .url();

    // Store the PKCE verifier to check it once the user authorizes
    // the application.
    let verifier = AuthResponseVerifier {
        csrf_token,
        pkce_verifier,
    };
    save_request(verifier, &env).await?;

    Response::redirect(auth_url)
}
