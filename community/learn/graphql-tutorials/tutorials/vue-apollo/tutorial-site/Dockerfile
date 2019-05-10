FROM node:carbon

# Install global dependencies
RUN npm -g install gatsby-cli

# clone gatsby-gitbook-boilerplate
RUN git clone https://github.com/hasura/gatsby-gitbook-boilerplate.git

# Create app directory
WORKDIR /gatsby-gitbook-boilerplate

RUN cd /gatsby-gitbook-boilerplate

# Install dependencies
RUN npm install

# Remove already existing dummy content
RUN rm -R content

# Bundle app source
COPY . .

# Build static files
RUN npm run build

# serve dist folder on port 8080
CMD ["gatsby", "serve", "--verbose", "--prefix-paths", "-p", "8080", "--host", "0.0.0.0"]
