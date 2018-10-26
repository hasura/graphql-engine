# JWT Authentication server Boilerplate

Sample JWT Authentication server for generating a JWT to use in the `Authentication` header by the built in JWT decoder in Hasura GraphQL Engine when started in JWT mode.

## Getting Started

### Deploy locally

#### Local Prerequisites

-   PostgreSQL up and accepting connections
-   Hasura GraphQL engine up and accepting connections
-   Node.js 8.9+ installed

#### Local instructions

Clone the Hasura Graph QL repository

```bash
git clone https://github.com/hasura/graphql-engine
```

Change directory to the JWT authentication server project root

```bash
cd graphql-engine/community/boilerplates/jwt-authentication-server
```

Install NPM dependencies

```bash
npm install
```

Set DATABASE_URL env

```bash
export DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>
```

Apply migrations via the Hasura GraphQL engine console _or_ via the CLI

##### CLI method

First install the Hasura CLI using the instructions here <https://docs.hasura.io/1.0/graphql/manual/hasura-cli/install-hasura-cli.html>

When the CLI is available set the endpoint of the running graphQL engine and run the migrations.

```bash
echo "endpoint: http://auth-graphql-engine:8080" > config.yaml
hasura migrate apply
```

##### Add user schema manually

Use the instructions <https://docs.hasura.io/1.0/graphql/manual/index.html> add a `users` table with the following fields using the console

| name       | type    | nullable | unique | default | primary |
| ---------- | ------- | -------- | ------ | ------- | ------- |
| id         | Integer | no       | yes    |         | yes     |
| username   | Text    | no       | yes    |         | no      |
| password   | Text    | no       | no     |         | no      |
| token      | Text    | no       | no     |         | no      |
| created_at | Date    | no       | no     | now()   |         |

Then start your app

```bash
    ENCRYPTION_KEY=<put your secret key here> npm start
```

### Deploy with Docker

#### Docker Prerequisites

-   Docker installed

#### Docker instructions

To understand fully how to use JWT for authentication there is a docker-compose file in the project root that will start a JWT authentication server, an authentication database, and authentication GraphQL engine and an a GraphQL engine protected by JWT authentication.

From the project root configure and start the docker containers

```bash
docker-compose -d
```

Follow the `usage` instructions below to set up a user and get a token, add this token to the `Authorization` header of the GraphQL requests

The JWT auth server located <http://localhost:8080>

The GraphQL engine console for the JWT protected app located at <http://localhost:8081>

The GraphQL engine console for the authentication database located at <http://localhost:8081>

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

In your GraphQL engine you will need to add permissions for a user named `user` with read permissions on the table and columns.

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
