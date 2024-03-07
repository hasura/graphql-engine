# build
FROM rust:1.75-buster AS builder

WORKDIR /app
COPY Cargo.toml Cargo.lock .
COPY src ./src

RUN cargo build --release

# copy
FROM ubuntu:jammy

COPY --from=builder /app/target/release/hasura-dev-auth-webhook /usr/bin

EXPOSE 3050
ENTRYPOINT ["/usr/bin/hasura-dev-auth-webhook"]
