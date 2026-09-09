# Casbin Middleware for Hasura GraphQL

This package provides a middleware for Hasura GraphQL that allows user to use Casbin for authorization.

Installation
To install the middleware, run the following command:
We are working on it. Example of npm package cane be : 

```bash
npm install casbin-hasura-middleware
```

## Configuration

Before using the middleware, user need to create a model.conf file that defines the Casbin model. This file should be placed in the root directory of user's Hasura GraphQL server.

Here is an example model.conf file:

```bash
[request_definition]
r = sub, obj, act

[policy_definition]
p = sub, obj, act

[policy_effect]
e = some(where (p.eft == allow))

[matchers]
m = r.sub == p.sub && r.obj == p.obj && r.act == p.act
```

## Usage

To use the middleware, user need to import it and add it to the middlewares array in user's Hasura GraphQL configuration file.
```bash
const { login, logout, auth } = require("./authMiddleware");

module.exports = {
  middlewares: [login, logout, auth]
  // Other configuration options
};
```

In user's GraphQL resolvers, user can then use the @auth directive to specify which operations require authorization. For example:

```bash
type Query {
  me: User @auth
}
```

This will require the current user to be authorized to perform the me query before it is allowed to run.




