const gql = require('graphql-tag');

const typeDefs = gql`
  type Query {
    hello:  String
  }
`;

const resolvers = {
    Query: {
        hello: () => "world",
    },
};

exports.typeDefs = typeDefs;
exports.resolvers = resolvers;
