# REST wrapper - Boilerplate to write a GraphQL server that wraps a REST API

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

## How to wrap

In our GraphQL service, we have defined a new API for each REST endpoint. This is what our mapping looks like:

| REST                |  GraphQL                                       |
|---------------------|------------------------------------------------|
| GET /users          | users (name: String) : [User]                  |
| GET /users/:userId  | getUser(id: String!): User                     |
| POST /users         | addUser(name: String!, balance: Int!): User    |

We would have to write a resolver for each API. This is what a typical resolver looks like, for e.g `getUser` :

```
getUser: async (_, { id }) => {
    return await getData(restAPIEndpoint + '/users/' + id);
}
```

## Deployment (Using Heroku)

You need a Heroku account and heroku-cli installed. Execute the following commands in a terminal:

1. Log into Heroku

```bash
heroku login
```

2. Create REST API app

```bash
# in my-rest-api directory (community/boilerplates/remote-schemas/rest-wrapper/my-rest-api)
heroku create
```

3. Deploy REST API app

```bash
git push heroku master
```

4. The above step will return an endpoint for your REST API. Update the constant `restAPIEndpoint` in `index.js` with this endpoint.

5. Create GRAPHQL API app

```bash
# in current directory (community/boilerplates/remote-schemas/rest-wrapper)
heroku create
```

6. Deploy GRAPHQL API app

```bash
git push heroku master
```

The final step will also return a HTTPS URL in the output. Now, you can go to Hasura console and add this URL as a Remote Schema to allow querying it via Hasura.
