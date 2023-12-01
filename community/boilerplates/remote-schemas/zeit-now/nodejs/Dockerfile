FROM node:20.9

WORKDIR /server

COPY ./package.json /server/

RUN npm install

COPY . /server/

CMD ["npm", "start"]
