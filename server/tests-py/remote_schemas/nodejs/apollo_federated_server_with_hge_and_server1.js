const { ApolloServer } = require('apollo-server');
const { ApolloGateway } = require("@apollo/gateway");

const gateway = new ApolloGateway({
    serviceList: [
        { name: 'hge', url: process.env.HGE_URL + "/v1/graphql" },
        { name: 'other', url: 'http://localhost:4003/' }
    ],
    introspectionHeaders: {
        'x-hasura-admin-secret': process.env.HASURA_GRAPHQL_ADMIN_SECRET
    }
});

const server = new ApolloServer({
    gateway,
    subscriptions: false
});

server.listen(4004).then(({ url }) => {
    console.log(`🚀 Server ready at ${url}`);
});
