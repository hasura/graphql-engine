# Zeit + Python + Flask + Graphene + SqlAlchemy GraphQL boilerplate

A boilerplate Python GraphQL Server using
[Flask](https://github.com/graphql-python/flask-graphql) and
[Graphene](https://github.com/graphql-python/graphene).

## Stack

Python 2.7

Postgres

Zeit

#### Frameworks/Libraries

Flask (Web framework)

Graphene (GraphQL framework)

SqlAlchemy (Postgres ORM)

## Local Development

The sample source code is present in `server.py`. Clone the repo and go to `community/boilerplates/remote-schemas/remote-schema/zeit-now/python/flask-graphene-sqlalchemy` folder:

```bash
$ git clone git@github.com:hasura/graphql-engine
$ cd graphql-engine/community/boilerplates/remote-schemas/remote-schema/zeit-now/python/flask-graphene-sqlalchemy
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
pip install -r requirements.txt

export FLASK_APP=server.py
flask run
```

Running the server using Docker:

```bash
docker build -t python-flask-graphene .
docker run -p 5000:5000 python-flask-graphene
```

This will start a local server on `localhost:5000`. You can hit the graphql service at `localhost:5000/graphql` which opens GraphiQL.

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
curl https://python-flask-graphene-vynveodwau.now.sh/graphql \
     -H 'Content-Type:application/json' \
     -d'{"query":"{ hello }"}'

{"data":{"hello":"World"}}
```

You can also visit the now url to open GraphiQL.
