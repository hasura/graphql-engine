# REST wrapper - Boilerplate to wrap a REST API as a GraphQL service

This boilerplate gives an example of writing a GraphQL service to wrap some pre-existing REST API.
You can add this REST wrapper as a Remote Schema in Hasura.

## Stack

Node 8.10

Apollo Server (GraphQL framework)

## REST API

The REST API is implemented in [my-rest-api](my-rest-api/) folder. It has the following APIs:

```
GET /users
GET /users/:userId
POST /users
```

The `GET /users` endpoint also takes an optional query param i.e. `GET /users?name=abc` and the `POST /users` endpoint expects a valid JSON payload in the body.

## GraphQL API

We will convert the above REST API into the following GraphQL API:

```
type User {
  id:       String!
  name:     String!
  balance:  Int!
}

type Query {
  getUser(id: String!): User
  users(name: String): [User]
}

type Mutation {
  addUser(name: String!, balance: Int!): User
}
```

## Deployment (Using Heroku)

#### Pre-requisites

node8

heroku-cli

#### Steps

1. Log into Heroku

```bash
# in current-directory (community/boilerplates/remote-schemas/rest-wrapper)
$ heroku login
```

2. Create app

```bash
$ heroku create
```

3. Git push

```bash
$ git push heroku master
```

The final step above will also return a HTTPS URL in the output. Now, you can go to Hasura console and add this URL as a Remote Schema.
