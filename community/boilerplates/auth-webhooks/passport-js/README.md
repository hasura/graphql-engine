# Passport.js Auth Webhook Boilerplate

This is a sample auth webhook for authenticating requests to the Hasura GraphQL Engine. This boilerplate also exposes login and signup endpoints.

Prerequisites
-------------

- PostgreSQL
- Node.js 8.9+

Getting Started
---------------

### Deploy locally

```bash
# Clone the repo
git clone https://github.com/hasura/graphql-engine

# Change directory
cd graphql-engine/community/boilerplates/auth-webhooks/passport-js

# Install NPM dependencies
npm install

# Set DATABASE_URL env
export DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>

# Apply migrations
# (Note) this step creates a "users" table in the database
knex migrate:latest

# Then simply start your app
npm start
```

### Deploy with Heroku

```bash
 # Create heroku app
 heroku create <app-name>

 # Create PostgreSQL addon
 heroku addons:create heroku-postgresql:hobby-dev -a <app-name>

 # Add git remote
 git remote add heroku https://git.heroku.com/<app-name>.git

 # Push changes to heroku
 # Note: You need to run this command from the toplevel of the working tree (graphql-engine)
 git subtree push --prefix community/boilerplates/auth-webhooks/passport-js heroku master

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
  "username": "test123",
  "token": "4ffd5ee92853787836325dcea74c02e4"
}
```

We can also use `/login` API to fetch the user token,
```bash
curl -H "Content-Type: application/json" \
     -d'{"username": "test123", "password": "test123"}' \
     http://localhost:8080/login
```

### Webhook for GraphQL Engine

Auth webhook that can be configured with Hasura GraphQl Engine is available at `/webhook`. It accepts Authorization header to validate the token against an user.

The client just need to add `Authorization: Bearer <token>` to the request sending to GraphQL Engine.

The endpoint (say `http://localhost:8080/webhook`) can be given as an environment variable `HASURA_GRAPHQL_AUTH_HOOK` to GraphQL Engine.

[Read more about webhook here](https://docs.hasura.io/1.0/graphql/manual/auth/authentication/webhook.html).
