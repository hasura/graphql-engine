# Used for preview, staging, and production docs builds

FROM node:24-alpine

ARG RELEASE_MODE=staging
ENV release_mode=${RELEASE_MODE}

ENV BUILD_VERSION="2.0"

RUN apk add --no-cache git \
    bash

WORKDIR /graphql-engine/docs

# Bundle app source
COPY . .

RUN node .yarn/releases/yarn-4.17.1.cjs install --immutable

# The docs build needs more than Node's default 2 GiB heap.
RUN NODE_OPTIONS=--max-old-space-size=4096 node .yarn/releases/yarn-4.17.1.cjs build

CMD ["node", ".yarn/releases/yarn-4.17.1.cjs", "serve", "-p", "8080", "--host", "0.0.0.0"]
