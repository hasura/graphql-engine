# Build the plugin binary
# Match Rust version used elsewhere (see rust-toolchain.yaml and dev-auth-webhook.Dockerfile)
FROM rust:1.88.0 AS builder

WORKDIR /app
COPY ./Cargo.toml ./Cargo.toml
COPY ./crates ./crates

# Build only the plugin package in release mode
RUN cargo build --release --package=pre-parse-plugin-example

# Runtime image
FROM debian:bookworm-slim

COPY --from=builder /app/target/release/pre-parse-plugin-example /usr/bin

# Install any needed runtime deps (mirroring dev-auth-webhook style)
RUN apt-get update && \
  apt-get install -y openssl && \
  rm -rf /var/lib/apt/lists/*

EXPOSE 5003
ENTRYPOINT ["/usr/bin/pre-parse-plugin-example"]

