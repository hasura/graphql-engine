FROM node:carbon

ENV NODE_ENV=PRODUCTION
ENV REACT_APP_CALLBACK_URL=https://react-apollo-todo-demo.hasura.app/callback 
# Create app directory
WORKDIR /app

# Install app dependencies
RUN npm -g install serve
# A wildcard is used to ensure both package.json AND package-lock.json are copied
COPY package*.json ./

RUN npm install

# Bundle app source
COPY . .

#Build react/vue/angular bundle static files
RUN npm run build

EXPOSE 8080
# serve dist folder on port 8080
CMD ["serve", "-s", "build", "-p", "8080"]
