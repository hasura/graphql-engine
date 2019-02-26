FROM node:11-slim

#RUN apt-get update && apt-get install -y \
#    curl \
#    gnupg \
#    libpq5
#
#RUN curl -sL https://deb.nodesource.com/setup_11.x | bash - \
#    && apt-get install -y nodejs \
#    && npm install -g yarn

RUN curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | INSTALL_PATH=/bin bash

WORKDIR /

COPY api-server /api-server
RUN yarn --cwd /api-server install

COPY hasura /hasura
COPY run.sh run.sh
COPY test_query.json test_query.json
