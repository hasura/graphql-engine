const { ApolloServer } = require('apollo-server');
const express = require('express');
const app = express();

const { typeDefs, resolvers } = require('./index');

const helloSchema = new ApolloServer({ typeDefs, resolvers });

helloSchema.listen().then(({ url }) => {
    console.log(`schema ready at ${url}`);
});
