# Authentication with JWT, Hasura claims and multiple roles

This is a sample auth JWT service for authenticating requests to the Hasura GraphQL Engine. This also exposes login and signup endpoints. Note that this repository can also be used in webhook mode in using the `/webhook` endpoint. The specifics of this repository is that it maps a `user_role` table to generate `x-hasura-allowed-roles` in the JWT claim so multiple roles can work with the Hasura Grapqh Engine as a backend of the application.

The endpoints to manage users are very limited (it is only possible to create a new user through the `/signup` endpoint). This is kind of a choice as this service is meant to be used for authentication only. The user and roles management can be done through the Hasura Graphql Engine or any other service accessing to the same database.

## Rationale

See this [issue](https://github.com/hasura/graphql-engine/issues/1420).

## Database schema

Three tables are used:

- `user`:
  - `id`: UUID. Primary key. Automatically generated.
  - `username`: String. Unique user identifier.
  - `password`: String. Hashed with bcrypt.
  - `active`: Boolean. If not active, not possible to connect with this user.
- `role`:
  - `id`: UUID. Primary key. Automatically generated.
  - `name`: String. Unique role identifier.
- `user_role`:
  - `id`: UUID. Primary key. Automatically generated.
  - `role_id`: UUID. Foreign key that references the `id` of the `role` table.
  - `user_id`: UUID. Foreign key that references the `id` of the `user` table.

## Prerequisites

- PostgreSQL
- Node.js 8.9+

## Getting Started

### Environment variables

_Note: you can find examples of RSA keys in the repository. **DO NOT USE THEM FOR PRODUCTION!**_

- `AUTH_PRIVATE_KEY="-----BEGIN RSA PRIVATE KEY-----\nypPTIfSzZ399o........"`

  RSA private key used to sign the JWT. You need to escape the lines with "\n" in the variable. If the variable is not set, it will try to use the private.pem file.

- `AUTH_PUBLIC_KEY="-----BEGIN PUBLIC KEY-----\nV02/4RJi........"`

  RSA private key used to deliver the JWK set. You need to escape the lines with "\n" in the variable. Please not that this feature is not working yet. If the variable is not set, it will try to use the public.pem file.

- `AUTH_KEY_ID="<unique-id-for-this-key>"`

  Used to identify the key currently used to sign the tokens. If the variable is not set, a hash string will be generated from the public key and used instead.

- `DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>`

  URL to connect to the Postgres database. The format is . For instance: `DATABASE_URL=postgres://postgres:@localhost:5432/postgres`

- `PORT=8080`

The port the server will listen to.

### Build and deploy on Docker (production)

First you need to build the image and to tag it:

```bash
docker build . -t hasura/passportjs-jwt-roles:latest
```

TODO: document on how to deploy on docker.

You can also have a look at [this docker-compose gist](https://gist.github.com/plmercereau/b8503c869ffa2b5d4e42dc9137b56ae1) to see how I use this service in a docker stack with Hasura and [Traefik](https://traefik.io/).

### Deploy locally (developpment)

```bash
# Clone the repo
git clone https://github.com/hasura/graphql-engine

# Change directory
cd community/boilerplates/auth-servers/passportjs-jwt-roles

# Install NPM dependencies
npm install

# Generate the RSA keys
openssl genrsa -out private.pem 2048
openssl rsa -in private.pem -pubout > public.pem

# print the keys in an escaped format
awk -v ORS='\\n' '1' private.pem
awk -v ORS='\\n' '1' public.pem

export DATABASE_URL=postgres://postgres:@localhost:5432/postgres

# Apply migrations
# (Note) this step creates tables "users", "roles" and "user_roles" in the database
knex migrate:latest

# Then simply start your app
npm start
```

<!-- ### Deploy with Heroku

TODO: test deployment with heroku, and rewrite this part

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

 # Apply migrations
# (Note) this step creates a "users" table in the database
 heroku run knex migrate:latest
``` -->

### Configure the Hasura GraphQL Engine

Run the Hasura GraphQL engine with `HASURA_GRAPHQL_JWT_SECRET` set like this:

```json
{ "type": "RS256", "key": "<AUTH_PUBLIC_KEY>" }
```

Where `<AUTH_PUBLIC_KEY>` is your RSA public key in PEM format, with the line breaks escaped with "\n".

You can also configure the server in JWKS mode and set `HASURA_GRAPHQL_JWT_SECRET` like this:

```json
{ "type": "RS256", "jwk_url": "hostname:port/jwks" }
```

More information in the [Hasura documentation](https://docs.hasura.io/1.0/graphql/manual/auth/authentication/jwt.html).

## Usage

### Signup

Once deployed or started locally, we can create an user using `/signup` API like below:

```bash
curl -H "Content-Type: application/json" \
     -d'{"username": "test123", "password": "test123", "confirmPassword": "test123"}' \
     http://localhost:8080/signup
```

On success, we get the response:

```json
{
  "id": "907f0dc7-6887-4232-8b6e-da3d5908f137",
  "username": "test123",
  "roles": ["user"],
  "token": "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1lIjoicGlsb3UiLCJodHRwczovL2hhc3VyYS5pby9qd3QvY2xhaW1zIjp7IngtaGFzdXJhLWFsbG93ZWQtcm9sZXMiOlsibWFuYWdlciIsInVzZXIiXSwieC1oYXN1cmEtZGVmYXVsdC1yb2xlIjoidXNlciIsIngtaGFzdXJhLXVzZXItaWQiOiI5MDdmMGRjNy02ODg3LTQyMzItOGI2ZS1kYTNkNTkwOGYxMzcifSwiaWF0IjoxNTQ4OTI5MTY2LCJleHAiOjE1NTE1MjExNjYsInN1YiI6IjkwN2YwZGM3LTY4ODctNDIzMi04YjZlLWRhM2Q1OTA4ZjEzNyJ9.hoY-lZ-6rbN_WVFy0Taxbf6QCtDPaTm407l6opv2bz-Hui9T7l7aafStsx9w-UscWUFWHpeStIo1ObV-lT8-j9t-nw9q5fr8wuO2zyKBMXjhD57ykR6BcKvJQMxE1JjyetVLHpj5r4mIb7_kaA8Dj8Vy2yrWFReHXDczYpQGc43mxxC05B5_xdScQrSbs9MkgQRh-Z5EknlLKWkpbuxPvoyWcH1wgLum7UABGNO7drvmcDDaRk6Lt99A3t40sod9mJ3H9UqdooLOfBAg9kcaCSgqWDkmCLBwtM8ONbKZ4cEZ8NEseCQYKqIoyHQH9vbf9Y6GBaJVbBoEay1cI48Hig"
}
```

### Login

Let's use the `/login` endpoint to fetch the user information and JWT:

```bash
curl -H "Content-Type: application/json" \
     -d'{"username": "test123", "password": "test123"}' \
     http://localhost:8080/login
```

It will then send back user information including the JWT in the same format as the above `/signup` endoint.

You can use this boilerplate as a webhook server in using the `/webhook` endpoint to fetch a webhook token:

```bash
curl -H "Content-Type: application/json" \
     -d'{"username": "test123", "password": "test123"}' \
     http://localhost:8080/login
```

## Limitations

- Not tested with Heroku
- There is no user and role management except to create a single user with no specific role. I myself do this part with a frontend app that access the database through a Hasura GraphQL endpoint.
- This server is designed to work with one RSA key only, and does not handle its regular rotation.
- No handling of JWT expiration and key turnover.
- This server is not (yet?) designed to handle authentication through other services such as Google, GitHub... It would be nice to do so, but to keep this server as a proxy that would add the Hasura claims in querying the database about the roles of the user. Comments or any contribution are welcome as well on this one.
- No automated tests.
- another cool feature to be would be to expose the endpoints through hasura remote schema, and not directly to the client

## Credits

The original repository can be found [here](https://github.com/platyplus/authentication-server).

This repository is inspired from the original [auth-webhooks/passport-js repo](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/auth-webhooks/passport-js).

Contributions are welcome!
