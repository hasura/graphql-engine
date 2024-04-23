# See https://github.com/LukeMathWalker/cargo-chef
# this should match the Rust version in rust-toolchain.yaml and the
# dev-auth-webhook
FROM rust:1.77.0 as chef

WORKDIR app

ENV DEBIAN_FRONTEND=noninteractive

RUN set -ex;\
    apt-get update; \
    apt-get install --no-install-recommends --assume-yes \
      curl git jq pkg-config ssh \
      libssl-dev lld protobuf-compiler

# Set up a directory to store Cargo files.
ENV CARGO_HOME=/app/.cargo
ENV PATH="$PATH:$CARGO_HOME/bin"
# Switch to `lld` as the linker.
ENV RUSTFLAGS="-C link-arg=-fuse-ld=lld"
# Building with build.rs requires the Git context to be available across volumes.
ENV GIT_DISCOVERY_ACROSS_FILESYSTEM=1

# Install Rust tools.
RUN cargo install cargo-chef cargo-nextest critcmp grcov just
COPY rust-toolchain.toml .
RUN rustup show

###
# Plan recipe
FROM chef AS planner

# Copy files
COPY .cargo ./.cargo
COPY Cargo.toml Cargo.lock ./
COPY crates ./crates

# Prepare the recipe
RUN cargo chef prepare --recipe-path recipe.json

###
# Build recipe
FROM chef AS builder

COPY --from=planner /app/recipe.json recipe.json
COPY --from=planner /app/.cargo/config.toml /app/.cargo/config.toml

# Build dependencies - this is the caching Docker layer!
RUN cargo chef cook --release --all-targets --recipe-path recipe.json

###
# Builds the application
FROM builder AS built

# Copy files
COPY .cargo ./.cargo
COPY Cargo.toml Cargo.lock ./
COPY crates ./crates
COPY .git ./.git

# Build the app
RUN cargo build --release --all-targets

###
# Ship the app in an image with `curl` and very little else
FROM ubuntu:jammy

# Install `curl` for health checks
RUN set -ex; \
    apt-get update; \
    apt-get install --assume-yes curl

# Install the engine
COPY --from=built /app/target/release/engine /usr/local/bin
ENTRYPOINT ["engine"]
