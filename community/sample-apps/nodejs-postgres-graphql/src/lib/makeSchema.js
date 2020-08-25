const { ApolloLink } = require("apollo-link");
const { onError } = require("apollo-link-error");
const { HttpLink } = require("apollo-link-http");
const fetch = require("node-fetch");
const {
  introspectSchema,
  makeRemoteExecutableSchema
} = require("graphql-tools");

const httpLink = new HttpLink({
  credentials: "include",
  uri: process.env.GQL_URI,
  headers: {
    "Auth-Type": "session",
    "content-type": "application/json",
    "x-hasura-admin-secret": process.env.HASURA_SECRET
  },
  fetch
});

const link = ApolloLink.from([
  onError(({ graphQLErrors, networkError }) => {
    if (graphQLErrors) {
      graphQLErrors.map(({ message, locations, path }) =>
        console.log(
          `[GraphQL error]: Message: ${message}, Location: ${locations}, Path: ${path}`
        )
      );
    }
    if (networkError) {
      console.log(`[Network error]: ${networkError}`);
    }
  }),
  httpLink
]);

const makeSchema = async () => {
  const schema = await introspectSchema(link);

  const executableSchema = makeRemoteExecutableSchema({
    schema,
    link
  });

  return executableSchema;
};

module.exports = {
  makeSchema
};
