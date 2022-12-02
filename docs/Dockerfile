# Used only for preview builds

FROM --platform=linux/amd64 node:17-alpine

ENV release_mode="staging"

ENV BUILD_VERSION="2.0"

RUN apk add --update git \
    bash \
    yarn

#WORKDIR /app
WORKDIR /graphql-engine/docs

# Bundle app source
COPY . .

RUN yarn

RUN yarn build

CMD ["yarn", "run", "serve", "-p", "8080", "--host", "0.0.0.0"]