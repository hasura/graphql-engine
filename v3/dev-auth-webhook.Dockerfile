# This should match the Rust version in rust-toolchain.yaml and the other Dockerfiles.
FROM rust:1.85.0 AS builder

WORKDIR /app
COPY ./Cargo.toml ./Cargo.toml
COPY ./crates ./crates

WORKDIR /app/crates/auth/dev-auth-webhook
RUN cargo build --release --package=dev-auth-webhook

# copy
FROM debian:bookworm-slim

COPY --from=builder /app/target/release/dev-auth-webhook /usr/bin

RUN apt-get update && \
  apt-get install -y openssl

EXPOSE 3050
ENTRYPOINT ["/usr/bin/dev-auth-webhook"]
