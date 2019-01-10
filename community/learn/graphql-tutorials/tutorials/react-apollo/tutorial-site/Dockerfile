FROM node:carbon

# Create app directory
WORKDIR /app

# Install app dependencies
# RUN npm -g install serve
RUN npm -g install gatsby-cli
# A wildcard is used to ensure both package.json AND package-lock.json are copied
COPY package.json ./

RUN npm install

# Bundle app source
COPY . .

ENV GATSBY_PATH_PREFIX='/react-apollo'

#Build react/vue/angular bundle static files
RUN npm run build
RUN cp -r public react-apollo
RUN mv react-apollo public/

EXPOSE 8080
# serve dist folder on port 8080
# CMD ["serve", "-s", "public", "-p", "8080"]

CMD ["gatsby", "serve", "--verbose", "--prefix-paths", "-p", "8080"]
