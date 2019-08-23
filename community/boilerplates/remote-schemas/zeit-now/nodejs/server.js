const { ApolloServer } = require('apollo-server');
const { makeExecutableSchema } = require('graphql-tools');

const port = process.env.PORT || 4000;

const typeDefs = `
  type Query {
    hello:  String
  }
`;

const resolvers = {
    Query: {
        hello: () => "world",
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
