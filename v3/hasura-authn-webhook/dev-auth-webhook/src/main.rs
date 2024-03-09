use std::collections::HashMap;
use std::env;
use std::net;

use axum::{response::Json, routing::post, Router};
use serde_json::Value;

const DEFAULT_PORT: u16 = 3050;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app = Router::new().route("/validate-request", post(validate_request));

    let host = net::IpAddr::V4(net::Ipv4Addr::UNSPECIFIED);
    let port = env::var("PORT")
        .map(|str| str.parse())
        .unwrap_or(Ok(DEFAULT_PORT))?;
    let address = (host, port).into();
    let server = axum::Server::bind(&address).serve(app.into_make_service());

    println!("Dev webhook authentication listening at {address}");

    server.await?;
    Ok(())
}

async fn validate_request(
    Json(payload): Json<HashMap<String, HashMap<String, String>>>,
) -> Json<Value> {
    Json(serde_json::to_value(payload.get("headers").unwrap()).unwrap())
}
