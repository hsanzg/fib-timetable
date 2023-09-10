mod auth;
mod calendar;
mod client;

use crate::auth::get_access_token;
use crate::calendar::export_calendar;
use crate::client::create_client;
use oauth2::{AuthorizationCode, CsrfToken, TokenResponse};
use std::borrow::Cow;
use worker::*;

/// Reads and parses a URL from an environment variable.
fn env_var_url(env: &Env, name: &str) -> Result<Url> {
    let value = env.var(name)?.to_string();
    Url::parse(&value).map_err(Into::into)
}

/// Returns the value of a given query parameter in a URL.
fn url_param<'a>(url: &'a Url, key: &str) -> Option<Cow<'a, str>> {
    let mut params = url.query_pairs();
    Some(params.find(|(k, _v)| k == key)?.1)
}

async fn handle_export(req: Request, env: Env) -> Result<Response> {
    // Trade the authorization code for an access token.
    let req_url = req.url()?;
    let auth_code = match url_param(&req_url, "code") {
        Some(c) => AuthorizationCode::new(c.to_string()),
        None => return Response::error("missing 'code' query parameter", 400),
    };
    let csrf_token = match url_param(&req_url, "state") {
        Some(v) => CsrfToken::new(v.to_string()),
        None => return Response::error("missing 'state' query parameter", 400),
    };

    let token_fut = get_access_token(auth_code, csrf_token, &env);
    let token_resp = match token_fut.await? {
        Some(resp) => resp,
        None => return Response::error("could not obtain access token", 403),
    };

    let access_token = token_resp.access_token();
    let client = create_client(access_token, &env)?;
    export_calendar(&client, &env).await
}

/// Handle a CORS pre-flight request.
fn handle_cors() -> Result<Response> {
    let mut headers = Headers::new();
    headers.set("Access-Control-Allow-Origin", "*")?;
    headers.set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")?;
    headers.set("Access-Control-Allow-Headers", "Content-Type")?;

    Response::empty().map(|res| res.with_headers(headers))
}

#[event(fetch)]
async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    Router::new()
        .get_async(
            "/",
            |_, ctx| async move { auth::handle_auth(ctx.env).await },
        )
        .get_async("/export", |req, ctx| async move {
            handle_export(req, ctx.env).await
        })
        //.options("*", |_, _| handle_cors())
        .run(req, env)
        .await
}
