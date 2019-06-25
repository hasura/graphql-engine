FROM node:carbon

# update this line when gatsby-gitbook-starter repo changes
RUN sh -c 'echo -e "Updated at: 2019-06-03 19:00:00 IST"'

# Install global dependencies
RUN npm -g install gatsby-cli

# clone gatsby-gitbook-starter
RUN git clone https://github.com/hasura/gatsby-gitbook-starter.git

# Create app directory
WORKDIR /gatsby-gitbook-starter

RUN cd /gatsby-gitbook-starter

# Install dependencies
RUN npm install

# Remove already existing dummy content
RUN rm -R content

# Bundle app source
COPY . .

# serve on port 8080
CMD ["gatsby", "develop", "--verbose", "-p", "8080", "--host", "0.0.0.0"]
