FROM node:11-slim

RUN apt-get update && apt-get install -y \
    libpq5 \
    netcat \
    git \
    cmake \
    pkgconf \
    && curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | INSTALL_PATH=/bin bash \
    && git clone https://github.com/google/brotli.git && cd brotli && mkdir out && cd out && ../configure-cmake \
    && make && make test && make install && ldconfig \
