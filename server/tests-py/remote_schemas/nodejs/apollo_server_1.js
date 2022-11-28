const { ApolloServer, gql } = require('apollo-server');
const { buildSubgraphSchema } = require('@apollo/subgraph');

const user = [
  {
    id: 1,
    city: 'New York'
  },
  {
    id: 2,
    city: 'Bangalore'
  },
  {
    id: 3,
    city: 'Melbourne'
  },
  {
    id: 4,
    city: 'New Delhi'
  }
];

const typeDefs = gql`
  extend schema
    @link(url: "https://specs.apollo.dev/federation/v2.0",
          import: ["@key", "@extends", "@external", "@shareable"])

  type Query {
    getUserData(id: Int!): user
  }

  type user @key(fields: "id") @extends {
    id: Int! @external
    city: String
  }
`;



const resolvers = {
  Query: {
    getUserData(parent, args, context, info) {
      return user.find(user => user.id === args.id);
    }
  }
}

const server = new ApolloServer({
  schema: buildSubgraphSchema({ typeDefs, resolvers })
});

server.listen({ port: process.env.PORT }).then(({ url }) => {
    console.log(`🚀 Server ready at ${url}`);
});
