# GraphQL server using python-flask-graphene

A boilerplate Python GraphQL Server using
[Flask](https://github.com/graphql-python/flask-graphql) and
[Graphene](https://github.com/graphql-python/graphene).

## Deploying

Clone the repo:

```bash
git clone https://github.com/hasura/graphql-engine
cd graphql-engine/community/boilerplates/graphql-servers/python-flask-graphene
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
curl https://python-flask-graphene-vynveodwau.now.sh/graphql \
     -H 'Content-Type:application/json' \
     -d'{"query":"{ hello }"}'

{"data":{"hello":"World"}}
```

You can also visit the now url to open GraphiQL:
[`https://python-flask-graphene-vynveodwau.now.sh/graphql`](https://python-flask-graphene-vynveodwau.now.sh/graphql).


## Running locally
Running the server locally:

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

GraphQL endpoint will be `http://localhost:5000/graphql`.

**Note**: When GraphQL Engine is running in a Docker container, `localhost` will
point to the containers local interface, not the host's interface. You might
have to use the host's docker host IP or a specific DNS label based on your OS.
