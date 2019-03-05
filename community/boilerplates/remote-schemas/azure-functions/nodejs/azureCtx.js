const { ApolloServer } = require("apollo-server-azure-functions");
const { typeDefs, resolvers } = require('./index');

const server = new ApolloServer({
    typeDefs,
    resolvers,
});

exports.handler = server.createHandler({
    cors: {
        origin: '*',
        credentials: true,
        allowedHeaders: 'Content-Type, Authorization'
    },
});

