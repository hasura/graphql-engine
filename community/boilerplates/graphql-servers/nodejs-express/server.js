const express = require('express');
const graphqlHTTP = require('express-graphql');
const { buildSchema } = require('graphql');

let count = 0;
const port = process.env.port || 3000;

// Construct a schema, using GraphQL schema language
const schema = buildSchema(`
  type Query {
    hello: String!
    count: Int!
  }

  type Mutation {
    increment_counter: count_mutation_response!
  }

  type count_mutation_response {
    new_count: Int!
  }
`);

// The root provides a resolver function for each API endpoint
const root = {
  hello: () => {
    return 'Hello world!';
  },
  count: () => {
    return count;
  },
  increment_counter: () => {
    return { new_count: ++count }
  }
};

var app = express();
app.use('/graphql', graphqlHTTP({
  schema: schema,
  rootValue: root,
  graphiql: true,
}));
app.listen(port);
console.log(`Running a GraphQL API server at localhost:${port}/graphql`);
