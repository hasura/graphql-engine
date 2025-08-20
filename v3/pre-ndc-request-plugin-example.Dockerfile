# Build the plugin binary
# Match Rust version used elsewhere (see rust-toolchain.yaml and dev-auth-webhook.Dockerfile)
FROM rust:1.86.0 AS builder

WORKDIR /app
COPY ./Cargo.toml ./Cargo.toml
COPY ./crates ./crates

# Build only the plugin package in release mode
RUN cargo build --release --package=pre-ndc-request-plugin-example

# Runtime image
FROM debian:bookworm-slim

COPY --from=builder /app/target/release/pre-ndc-request-plugin-example /usr/bin

# Install any needed runtime deps (mirroring dev-auth-webhook style)
RUN apt-get update && \
  apt-get install -y openssl && \
  rm -rf /var/lib/apt/lists/*

EXPOSE 5001
ENTRYPOINT ["/usr/bin/pre-ndc-request-plugin-example"]

