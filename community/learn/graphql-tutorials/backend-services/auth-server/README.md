# JWT Authentication server Boilerplate

Sample JWT Authentication server for generating a JWT to use in the `Authentication` header by the built in JWT decoder in Hasura GraphQL Engine when started in JWT mode.

## Getting Started

### Deploy locally

#### Local Prerequisites

-   PostgreSQL up and accepting connections
-   Hasura GraphQL engine up and accepting connections
-   Node.js 8.9+ installed

#### Local instructions

Install NPM dependencies

```bash
npm install
```

Set environment variables. Open `.env` file and add the following env

```bash
ENCRYPTION_KEY=<replace_it_with_your_JWT_SECRET>
DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>
PORT=8080
```

##### User Schema

The following `users` table is assumed to be present in your schema. The table can have additional fields too.

| name       | type    | nullable | unique | default | primary |
| ---------- | ------- | -------- | ------ | ------- | ------- |
| id         | Text    | no       | yes    |         | yes     |
| name       | Text    | no       | no     |         | no      |
| password   | Text    | no       | no     |         | no      |
| created_at | Date    | no       | no     | now()   |         |

Then start your app

```bash
npm start
```

## Usage

### Signup/Login

Once deployed or started locally, we can create an user using `/signup` API like below:

```bash
curl -H "Content-Type: application/json" \
     -d'{"username": "test123", "password": "test123"}' \
     http://localhost:8080/signup
```

On success, we get the response:

```json
{
  "id": 1
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
  http://localhost:8081/v1/graphql \
  -H 'Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxIiwibmFtZSI6InRlc3QxMjMiLCJpYXQiOjE1NDAzNzY4MTUuODUzLCJodHRwczovL2hhc3VyYS5pby9qd3QvY2xhaW1zIjp7IngtaGFzdXJhLWFsbG93ZWQtcm9sZXMiOlsiZWRpdG9yIiwidXNlciIsIm1vZCJdLCJ4LWhhc3VyYS11c2VyLWlkIjoiMSIsIngtaGFzdXJhLWRlZmF1bHQtcm9sZSI6InVzZXIiLCJ4LWhhc3VyYS1yb2xlIjoidXNlciJ9fQ.w9uj0FtesZOFUnwYT2KOWHr6IKWsDRuOC9G2GakBgMI' \
  -H 'Content-Type: application/json' \
  -d '{ "query": "{ table { column } }" }'
```
