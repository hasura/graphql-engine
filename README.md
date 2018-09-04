# GraphQL Custom Resolver Example

This is a simple example of using a custom resolver with Hasura's GraphQL API.

## Motivation

Hasura GraphQL Engine provides instant GraphQL APIs over the tables and views of any Postgres database. It also comes with a fine grained access control layer that helps you restrict the data that can be consumed.

However, sometimes you might want to have custom logic that is entirely unrelated to the database. In such cases, you can do one of the two things:
1. Use Postgres events to trigger serverless functions and run your asynchronous logic.
2. Write custom resolvers and merge them with the Hasura schema.

In this example, we illustrate how to write custom resolvers and merge them with the Hasura GraphQL Engine.

We combine Hasura GraphQL Engine's GraphQL API running at `https://bazookaand.herokuapp.com/v1alpha1/graphql` with the following:

1. A `hello` query
2. A `count` query (that returns a counter from another data source )
3. A `increment_counter` mutation that increments the value of `count`.
4. A `user_average_age` query that makes directly makes an SQL query to Postgres using knex.

You can use this as a boilerplate to write custom resolvers with Hasura GraphQL Engine.

## Usage

1. Enter the directory and install the required dependencies.

```bash
npm install
```

2. Set appropriate environment variables for the GraphQL Engien URL, the access key to GraphQL Engine and the Postgres connection string.


```bash
export HASURA_GRAPHQL_ENGINE_URL='https://hge.herokuapp.com'
export X_HASURA_ACCESS_KEY='<access_key>'
export PG_CONNECTION_STRING='<postgres-connection-string>'
```

3. Run the server

```bash
npm start
```

## Writing custom resolvers

We will use Apollo's `graphql-tools` library to make a working GraphQL Schema out of our custom resolvers. Finally, we will merge these resolvers with the existing Hasura schema so that it can eb queried under the same endpoint.

### Writing type definitions

The type definitions are written in standard GraphQL format. We need the following queries in our custom logic:


```graphql
type Query {
  # field hello will return "Hello World" which is a string
  hello: String,

  # field count will return an Int
  count: Int,

  #field user_average_age will return a Float
  user_average_age: Float
}

type Mutation {
  # field "increment_counter" will increment the counter and return type IncrementCounter
  increment_counter: IncrementCounter,

  # IncrementCounter simply returns the new value of the counter
  new_count: Int
}
```

### Writing resolvers

Every resolver is a function that is executed with the following arguements in the order below:

1. `root`: The root of the current field
2. `args`: The arguements provided in the query
3. `context`: The server context, which also consists of headers
4. `info`: The AST document related to the query made

The resolvers in our case are:

```js
const resolvers = {
  // resolvers for queries
  Query: {
    hello: (root, args, context, info) => {
      // return response
      return 'Hello world!';
    },
    count: (root, args, context, info) => {
      // return response
      return count;
    },
    user_average_age: async (root, args, context, info) => {
      // make SQL query using knex client
      const response = await knexClient('user')
        .avg('age');
      // return response
      return response[0].avg;
    }
  },

  // resolvers for mutations
  Mutation: {
    increment_counter: (root, args, context, info) => {
      // return response
      return { new_count: ++count };
    }
  }
};
```

