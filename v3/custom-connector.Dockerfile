# This should match the Rust version in rust-toolchain.yaml and the other Dockerfiles.
FROM rust:1.84.1

WORKDIR /app

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
COPY rust-toolchain.toml .
RUN rustup show

COPY Cargo.toml Cargo.lock .

RUN mkdir bin

# Build the binaries and tests
RUN --mount=type=cache,target=/app/.cargo/git --mount=type=cache,target=/app/.cargo/registry \
    --mount=type=cache,target=./target \
    --mount=type=bind,source=./crates,target=./crates \
    set -ex; \
    cargo build --bin custom-connector

# Copy the binaries out of the cache
RUN --mount=type=cache,target=./target \
    find target/debug -maxdepth 1 -type f -executable -exec cp '{}' bin \;
