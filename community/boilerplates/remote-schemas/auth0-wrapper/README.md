# Auth0 wrapper - Boilerplate to write a GraphQL server that wraps Auth0's REST API

This boilerplate gives an example of writing a GraphQL service to wrap [Auth0](http://auth0.com/)'s REST API.
You can add this Auth0 wrapper as a remote schema in Hasura.

## Stack

Node 14.4

Apollo Server (GraphQL framework)

## REST API

GET: `https://[YOUR AUTH0 ROOT].auth0.com/api/v2/users/{id}`

GET: `https://[YOUR AUTH0 ROOT].auth0.com/api/v2/users`

You can get a token via [Auth0's Management API](https://auth0.com/docs/api/management/v2).

## GraphQL API

We convert the above REST API into the following GraphQL API:

```
  type Query {
    auth0 (auth0_id: String, email: String): Auth0Info
  }

  type Auth0Info {
    user_id: String,
    email: String,
    email_verified: Boolean,
    name: String,
    picture: String,
    nickname: String,
    created_at: String,
    last_login: String,
    logins_count: Int
  }
```

## Deployment (Using Heroku)

You need a Heroku account and `heroku-cli` installed. Execute the following commands in a terminal:

1. Log into Heroku

```bash
heroku login
```

2. Create GRAPHQL API app

```bash
# in current directory (community/boilerplates/remote-schemas/auth0-wrapper)
heroku create
```

3. Deploy GRAPHQL API app

```bash
git push heroku master
```

The final step will also return a HTTPS URL in the output. Now, you can go to Hasura console and add this URL as a remote schema to allow querying it via Hasura.
