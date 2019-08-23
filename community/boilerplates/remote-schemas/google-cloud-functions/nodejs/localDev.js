const { ApolloServer } = require('apollo-server');
const { typeDefs, resolvers } = require('./index');

const server = new ApolloServer({ typeDefs, resolvers });

server.listen().then(({ url }) => {
    console.log(`schema ready at ${url}`);
});
