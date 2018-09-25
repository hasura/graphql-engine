# Passport.js Auth Webhook example for Hasura GraphQL Engine

This is a sample auth webhook for authenticating requests to the Hasura GraphQL Engine.

Prerequisites
-------------

- [PostgreSQL](https://www.postgresql.org/download/)
- [Node.js 8.9+](http://nodejs.org)

Getting Started
---------------

The easiest way to get started is to clone the repository:

```bash
# Get the latest snapshot
git clone git@github.com:hasura/graphql-engine.git myproject

# Change directory
cd myproject/community/boilerplates/auth-webhooks/passport-js-auth-webhook

# Install NPM dependencies
npm install

# Set DATABASE_URL env
export DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>

# Apply migrations
knex migrate:latest

# Then simply start your app
npm start
```

### Deploy with Heroku (recommended)

1. Follow the below steps to deploy with heroku:
  ```bash
   # Create heroku app
   heroku create <app-name>

   # Create PostgreSQL addon
   heroku addons:create heroku-postgresql:hobby-dev -a <app-name>

   # Add git remote
   git remote add heroku https://git.heroku.com/<app-name>.git

   # Push changes to heroku
   # Note: You need to run this command from the toplevel of the working tree
   git subtree push --prefix community/boilerplates/auth-webhooks/passport-js-auth-webhook heroku master

   # Apply migrations
   heroku run knex migrate:latest
  ```

2. Once it is deployed, we can create an user using `/signup` API like below:
  ```
     curl --header "Content-Type: application/json" \
      --request POST \
      --data '{"username": "test123", "password": "test123", "confirmPassword": "test123"}' \
      http://localhost:8080/signup
  ```

3. On signup, we get below response:
  ```json
    {
      "id": 1,
      "username": "test@hasura.io",
      "token": "4ffd5ee92853787836325dcea74c02e4"
    }
  ```

4. Also, we can use `/login` API to fetch the user token,
  ```
     curl --header "Content-Type: application/json" \
      --request POST \
      --data '{"username": "test123", "password": "test123"}' \
      http://localhost:8080/login
  ```

5. Webhook is available at `/webhook` API which accepts Authorization header to validate the token against an user.

## Usage with Hasura GraphQL Engine

Once you have deployed this webhook, you can use it along with the GraphQL engine. You have to set the webhook URL as an environment variable in the docker container that runs the GraphQL engine.

*[Read the docs](https://docs.hasura.io/1.0/graphql/manual/auth/webhook.html).*

Send the `token` as a header  while making queries to the `graphql-engine`

```
{
  "Authorization": "Bearer <token>"
}
```
