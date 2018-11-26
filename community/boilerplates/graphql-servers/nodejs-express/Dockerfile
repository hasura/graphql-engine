FROM node:8

WORKDIR /server

COPY ./package.json /server/

RUN npm install

COPY . /server/

CMD ["npm", "start"]
