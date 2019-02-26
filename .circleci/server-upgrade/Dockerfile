FROM node:11-slim

RUN apt-get update && apt-get install -y \
    libpq5 \
    netcat \
    && curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | INSTALL_PATH=/bin bash
