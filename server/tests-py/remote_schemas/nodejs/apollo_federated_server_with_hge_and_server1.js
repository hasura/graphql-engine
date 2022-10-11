const { ApolloServer } = require('apollo-server');
const { ApolloGateway } = require("@apollo/gateway");

const gateway = new ApolloGateway({
    serviceList: [
        { name: 'hge', url: process.env.HGE_URL + "/v1/graphql" },
        { name: 'other', url: process.env.OTHER_URL }
    ],
    introspectionHeaders: {
        'x-hasura-admin-secret': process.env.HASURA_GRAPHQL_ADMIN_SECRET
    }
});

const server = new ApolloServer({
    gateway,
    subscriptions: false
});

server.listen({ port: process.env.PORT }).then(({ url }) => {
    console.log(`🚀 Server ready at ${url}`);
});
