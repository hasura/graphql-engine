const fetch = require(`node-fetch`)
const { createHttpLink } = require(`apollo-link-http`)

module.exports = {
  plugins: [
    {
      resolve: 'gatsby-source-graphql',
      options: {
        typeName: 'HASURA',
        fieldName: 'hasura',
        createLink: (pluginOptions) => {
          return createHttpLink({
            uri: 'http://localhost:8080/v1/graphql',
            headers: {
            },
            fetch,
          })
        },
      },
    },
  ]
};
