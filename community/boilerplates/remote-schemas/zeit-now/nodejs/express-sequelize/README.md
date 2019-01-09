# GraphQL server using NodeJS-Express

A boilerplate Python GraphQL Server using NodeJS-Express using the official [graphql-js](https://graphql.github.io/graphql-js/running-an-express-graphql-server/) library.

## Deploying

Clone the repo:

```bash
git clone https://github.com/hasura/graphql-engine
cd graphql-engine/community/boilerplates/graphql-servers/nodejs-express
```

### Using Zeit Now

Install the [Zeit Now](https://zeit.co/now) CLI:

```bash
npm install -g now
```

Deploy the server:
```bash
now
```

Get the URL and make a sample query:
```bash
curl https://app-name-something.now.sh/graphql \
     -H 'Content-Type:application/json' \
     -d'{"query":"{ hello }"}'

{"data":{"hello":"Hello World!"}}
```

You can also visit the `/graphql` endpoint of the now url to open GraphiQL.

## Running locally
Running the server locally:

```bash
npm install
npm start
```

Running the server using Docker:

```bash
docker build -t nodejs-express-graphql .
docker run -p 4000:4000 nodejs-express-graphql
```

GraphQL endpoint will be `http://localhost:4000/graphql`.

**Note**: When GraphQL Engine is running in a Docker container, `localhost` will
point to the containers local interface, not the host's interface. You might
have to use the host's docker host IP or a specific DNS label based on your OS.
