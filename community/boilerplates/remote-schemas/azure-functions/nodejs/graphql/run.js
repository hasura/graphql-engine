const { ApolloServer } = require("apollo-server-azure-functions");
const { typeDefs, resolvers } = require('./server');

const server = new ApolloServer({
    typeDefs,
    resolvers,
});

module.exports = server.createHandler({
    cors: {
        origin: '*',
        credentials: true,
        allowedHeaders: 'Content-Type, Authorization'
    },
});

