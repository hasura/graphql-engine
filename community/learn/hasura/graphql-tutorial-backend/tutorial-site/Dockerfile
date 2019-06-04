FROM node:carbon

# update this line when gatsby-gitbook-boilerplate repo changes
RUN sh -c 'echo -e "Updated at: 2019-06-03 19:00:00 IST"'

# Install global dependencies
RUN npm -g install gatsby-cli

# clone gatsby-gitbook-boilerplate repo
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
