FROM node:8.9.1

RUN mkdir -p /opt/app

RUN chown node:node /opt/app

WORKDIR /opt/app

COPY package.json .

USER node

RUN npm install

COPY . .

CMD npm start
