# build
FROM rust:1.77 AS builder

WORKDIR /app
COPY ./Cargo.toml ./Cargo.toml
COPY ./crates/tracing-util ./crates/tracing-util
COPY ./crates/dev-auth-webhook ./crates/dev-auth-webhook

WORKDIR /app/crates/dev-auth-webhook
RUN cargo build --release --package=dev-auth-webhook

# copy
FROM debian:bookworm-slim

COPY --from=builder /app/target/release/dev-auth-webhook /usr/bin

RUN apt-get update && \
  apt-get install -y openssl

EXPOSE 3050
ENTRYPOINT ["/usr/bin/dev-auth-webhook"]
