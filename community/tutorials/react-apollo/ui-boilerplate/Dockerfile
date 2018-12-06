FROM node:carbon

# Create app directory
WORKDIR /app

# Install app dependencies
RUN npm -g install serve
# A wildcard is used to ensure both package.json AND package-lock.json are copied
COPY package*.json ./

RUN npm install

# Bundle app source
COPY src /app
#Build react/vue/angular bundle static files
RUN npm run build

EXPOSE 8080
# serve dist folder on port 8080
CMD ["serve", "-s", "dist", "-p", "8080"]
