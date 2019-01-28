const fetch = require(`node-fetch`)
const { createHttpLink } = require(`apollo-link-http`)

module.exports = {
  plugins: [
    {
      resolve: 'gatsby-source-graphql',
      options: {
        typeName: 'HASURA',
        fieldName: 'hasura',
        createLink: () =>
          createHttpLink({
            uri: `${ process.env.HASURA_GRAPHQL_URL }`,
            headers: {},
            fetch,
          }),
        refetchInterval: 10, // Refresh every 60 seconds for new data
      },
    },
  ]
};
