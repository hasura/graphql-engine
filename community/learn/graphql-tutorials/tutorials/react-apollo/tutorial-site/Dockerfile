FROM node:carbon

ENV GATSBY_SITE_TITLE='React Apollo Todo'
ENV GATSBY_SITE_DESCRIPTION='Tutorial'
ENV GATSBY_DOCS_LOCATION='https://github.com/hasura/graphql-engine/tree/master/community/tutorials/react-apollo'
ENV GATSBY_PATH_PREFIX='/react-apollo'
ENV GATSBY_HEADER_TITLE='React Apollo'

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

#Build react/vue/angular bundle static files
RUN npm run build
RUN cp -r public react-apollo
RUN mv react-apollo public/


EXPOSE 8080
# serve dist folder on port 8080
# CMD ["serve", "-s", "public", "-p", "8080"]
CMD ["gatsby", "serve", "--verbose", "--prefix-paths", "-p", "8080"]
