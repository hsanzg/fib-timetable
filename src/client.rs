use oauth2::AccessToken;
use reqwest::header::{HeaderMap, HeaderValue};
use reqwest::{header, Client};
use worker::*;

fn header_value(raw: &str) -> Result<HeaderValue> {
    HeaderValue::from_str(raw).map_err(|e| {
        let message = format!("header value should only contain ASCII characters: {e}");
        Error::RustError(message)
    })
}

pub fn create_client(access_token: &AccessToken, _env: &Env) -> Result<Client> {
    let mut headers = HeaderMap::new();

    // Attach the access token to all requests.
    let auth_value = format!("Bearer {}", access_token.secret());
    let mut auth_value = header_value(&auth_value)?;
    auth_value.set_sensitive(true);
    headers.insert(header::AUTHORIZATION, auth_value);

    // Request JSON responses from the API.
    headers.insert(header::ACCEPT, HeaderValue::from_static("application/json"));

    // Prefer English names and descriptions if available.
    headers.insert(header::ACCEPT_LANGUAGE, HeaderValue::from_static("en"));

    Client::builder()
        .default_headers(headers)
        .build()
        .map_err(|e| Error::RustError(e.to_string()))
}
