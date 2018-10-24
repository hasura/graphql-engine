FROM node:alpine

RUN mkdir -p /opt/app

WORKDIR /opt/app

COPY package.json .

RUN apk --no-cache add python \
  g++ gcc libgcc libstdc++ linux-headers make python

RUN npm install

COPY . .

CMD npx knex migrate:latest && npm start