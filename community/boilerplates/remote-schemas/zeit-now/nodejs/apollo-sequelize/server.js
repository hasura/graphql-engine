const { ApolloServer } = require('apollo-server');
const { makeExecutableSchema } = require('graphql-tools');

const port = process.env.PORT || 3000;

let count = 0;

const typeDefs = `
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
`;

const resolvers = {
  Query: {
    hello: () => {
      return "Hello World!"
    },
    count: () => {
      return count;
    }
  },
  Mutation: {
    increment_counter: () => {
      return { new_count: ++count }
    }
  }
};

const schema = makeExecutableSchema({
  typeDefs,
  resolvers
});

const server = new ApolloServer({
  schema
});

server.listen({ port }).then(({url}) => {
  console.log(`GraphQL server running at ${url}`);
});
