# build
FROM rust:1.77 AS builder

WORKDIR /app
COPY ./Cargo.toml ./Cargo.toml
COPY ./crates/tracing-util ./crates/tracing-util
COPY ./crates/hasura-authn-webhook/dev-auth-webhook ./crates/hasura-authn-webhook/dev-auth-webhook

WORKDIR /app/crates/hasura-authn-webhook/dev-auth-webhook
RUN cargo build --release

# copy
FROM debian:bookworm-slim

COPY --from=builder /app/crates/hasura-authn-webhook/dev-auth-webhook/target/release/hasura-dev-auth-webhook /usr/bin

RUN apt-get update && \
  apt-get install -y openssl

EXPOSE 3050
ENTRYPOINT ["/usr/bin/hasura-dev-auth-webhook"]
