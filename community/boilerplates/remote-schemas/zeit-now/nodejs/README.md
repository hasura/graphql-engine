# Zeit (1.0) + NodeJS + Apollo

A boilerplate using Nodejs and [Apollo Server](https://www.apollographql.com/docs/apollo-server/) that can be deployed on Zeit.

## Stack

node 8.10

Zeit 1.0

#### Frameworks/Libraries

Apollo Server (GraphQL framework)

## Local Development

The sample source code is present in `server.js`.

```bash
$ git clone git@github.com:hasura/graphql-engine
$ cd graphql-engine/community/boilerplates/remote-schemas/zeit-now/nodejs
```

Run the server locally:

```bash
npm install
npm start
```

Running the server using Docker:

```bash
docker build -t graphql .
docker run -p 4000:4000 graphql
```

This will start a local server on `localhost:4000`. You can hit the graphql service at `localhost:4000/graphql` which opens GraphiQL.

## Deployment

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

You can also visit the now url to open GraphiQL.
