FROM node:8.9.1

RUN mkdir -p /opt/app

RUN chown node:node /opt/app

WORKDIR /opt/app

COPY --chown=node:node package.json .

USER node

RUN npm install

COPY --chown=node:node . .

RUN curl https://graphql-tutorials.auth0.com/pem > graphql-tutorials.pem

CMD npm start