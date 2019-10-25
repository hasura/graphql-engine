const { ApolloServer } = require("apollo-server-express");
const { InMemoryCache } = require("apollo-cache-inmemory");

const { makeSchema } = require("../lib/makeSchema");

const Graphql = () => {
  const graphql = new ApolloServer({
    schema: makeSchema(),
    playground: true,
    introspection: true,
    cache: new InMemoryCache()
  });

  return graphql.getMiddleware({
    path: "/graphql" // defaults to `/graphql` itself
  });
};

const gqlRoute = Graphql();

module.exports = gqlRoute;