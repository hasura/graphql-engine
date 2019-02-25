FROM ubuntu:18.04

ARG abs_version=1.2.0

RUN curl -sL https://deb.nodesource.com/setup_11.x | bash - \
&& apt-get install -y nodejs \
&& npm install -g yarn

RUN apt-get install -y \
    curl \
    gnupg \
    libpq5

RUN curl -Lo /bin/abs https://github.com/abs-lang/abs/releases/download/${abs_version}/abs-${abs_version}-linux-amd64 \
    && chmod +x /bin/abs

RUN curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | INSTALL_PATH=/bin bash

WORKDIR /

COPY api-server /api-server
RUN cd /api-server && yarn install

COPY hasura /hasura
