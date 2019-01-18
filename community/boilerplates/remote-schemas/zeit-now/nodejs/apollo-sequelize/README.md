# Zeit + NodeJS + Apollo + Sequelize GraphQL boilerplate

A boilerplate using nodejs and [Apollo Server](https://www.apollographql.com/docs/apollo-server/) that can be deployed on Zeit.

## Stack

node 8.10

Postgres

Zeit Now

#### Frameworks/Libraries

Apollo Server (GraphQL framework)

Sequelize (Postgres ORM)

## Local Development

The sample source code is present in `server.js`. Clone the repo and go to `community/boilerplates/remote-schemas/remote-schema/zeit-now/nodejs/apollo-sequelize` folder:

```bash
$ git clone git@github.com:hasura/graphql-engine
$ cd graphql-engine/community/boilerplates/remote-schemas/remote-schema/zeit-now/nodejs/apollo-sequelize
```

1) First, let's set the environment variable for connecting to the postgres instance. This can be a local postgres instance or some managed postgres instance like AWS RDS.

```bash
$ export POSTGRES_CONNECTION_STRING='postgres://username:password@rds-database-endpoint.us-east-1.rds.amazonaws.com:5432/mydb' 
```

2) Next, lets create the tables required for our schema.

```bash
psql $POSTGRES_CONNECTION_STRING -c "create table users(id serial primary key, name text, balance integer); create table min_amount(amount integer); insert into min_amount values (100)" 
```

3) Now, you can run the server locally:

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
