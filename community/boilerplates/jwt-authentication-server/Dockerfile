FROM node:alpine

RUN mkdir -p /opt/app

RUN chown node:node /opt/app

WORKDIR /opt/app

COPY --chown=node:node package.json .

RUN apk --no-cache add python \
  g++ gcc libgcc libstdc++ linux-headers make python curl

RUN curl -L https://cli.hasura.io/install.sh | sed s/NEED_SUDO=0//g | sh

USER node

RUN npm install

COPY --chown=node:node . .

RUN echo "endpoint: http://auth-graphql-engine:8080" > config.yaml

CMD sleep 10 && hasura migrate apply && npm start