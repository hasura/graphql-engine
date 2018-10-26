# JWT Authentication server Boilerplate

Sample JWT Authentication server for generating a JWT to use in the `Authentication` header by the built in JWT decoder in Hasura GraphQL Engine when started in JWT mode.

## Getting Started

### Deploy locally

#### Local Prerequisites

-   PostgreSQL
-   Node.js 8.9+

#### Local instructions

```bash
# Clone the repo
git clone https://github.com/hasura/graphql-engine

# Change directory
cd graphql-engine/community/boilerplates/auth-webhooks/jwt-passport-js

# Install NPM dependencies
npm install

# Set DATABASE_URL env
export DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>

# Apply migrations
# (Note) this step creates a "users" table in the database
knex migrate:latest

# Then start your app
ENCRYPTION_KEY=<put your secret key here> npm start
```

### Deploy with Docker

#### Docker Prerequisites

-   Docker installed

#### Docker instructions

To understand fully how to use JWT for authentication there is a docker-compose file in the project root that will start a JWT authentication server and the Hasura GraphQL engine.

From the project root configure and start the docker containers

```bash
docker-compose -d
```

Follow the `usage` instructions below to set up a user and get a token, add this token to the `Authorization` header of the GraphQL requests

The JWT auth server located <http://localhost:8080>

The GraphQL engine console located at <http://localhost:8081>

### Deploy with Heroku

```bash
 # Create heroku app
 heroku create <app-name>

 # Create PostgreSQL addon
 heroku addons:create heroku-postgresql:hobby-dev -a <app-name>

 # Add git remote
 git remote add heroku https://git.heroku.com/<app-name>.git

 # Push changes to heroku
 # Note: You need to run this command from the toplevel of the working tree (graphql-enginej)
 git subtree push --prefix community/boilerplates/auth-webhooks/passport-js heroku master

 heroku config:get GITHUB_USERNAME

 heroku config:set ENCRYPTION_KEY=<put your secret key here>

 # Apply migrations
# (Note) this step creates a "users" table in the database
 heroku run knex migrate:latest
```

## Usage

### Signup/Login

Once deployed or started locally, we can create an user using `/signup` API like below:

```bash
curl -H "Content-Type: application/json" \
     -d'{"username": "test123", "password": "test123", "confirmPassword": "test123"}' \
     http://localhost:8080/signup
```

On success, we get the response:

```json
{
  "id": 1,
  "username": "test123"
}
```

We can also use `/login` API to fetch the user token:

```bash
curl -H "Content-Type: application/json" \
     -d'{"username": "test123", "password": "test123"}' \
     http://localhost:8080/login
```

On success, we get the response:

```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOjEsIm5hbWUiOiJ0ZXN0MTIzIiwiaWF0IjoxNTQwMjkyMzgyLjQwOSwiaHR0cHM6Ly9oYXN1cmEuaW8vand0L2NsYWltcyI6eyJ4LWhhc3VyYS1hbGxvd2VkLXJvbGVzIjpbImVkaXRvciIsInVzZXIiLCJtb2QiXSwieC1oYXN1cmEtdXNlci1pZCI6MSwieC1oYXN1cmEtZGVmYXVsdC1yb2xlIjoidXNlciJ9fQ.KtAUroqyBroBJL7O9og3Z4JnRkWNfr07cHQfeLarclU"
}
```

### Authenticate JWT using GraphQL Engine

The GraphQL engine comes with built in JWT authentication.  You will need to start the engine with the same secret/key as the JWT auth server using the environment variable `HASURA_GRAPHQL_JWT_SECRET` (HASURA_GRAPHQL_ACCESS_KEY is also required see the docs)

In your GraphQL engine you will need to add permissions for a user named `user` with read permissions on a table.

A sample CURL command using the above token would be:

```bash
curl -X POST \
  http://localhost:8081/v1alpha1/graphql \
  -H 'Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxIiwibmFtZSI6InRlc3QxMjMiLCJpYXQiOjE1NDAzNzY4MTUuODUzLCJodHRwczovL2hhc3VyYS5pby9qd3QvY2xhaW1zIjp7IngtaGFzdXJhLWFsbG93ZWQtcm9sZXMiOlsiZWRpdG9yIiwidXNlciIsIm1vZCJdLCJ4LWhhc3VyYS11c2VyLWlkIjoiMSIsIngtaGFzdXJhLWRlZmF1bHQtcm9sZSI6InVzZXIiLCJ4LWhhc3VyYS1yb2xlIjoidXNlciJ9fQ.w9uj0FtesZOFUnwYT2KOWHr6IKWsDRuOC9G2GakBgMI' \
  -H 'Content-Type: application/json' \
  -d '{ "query": "{ table { column } }" }'
```

See [the Hasura docs](https://docs.hasura.io/1.0/graphql/manual/auth/jwt.html) for more details
.
