module.exports = {
  // ...
  plugins: [
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        name: 'queries',
        path: `${__dirname}/src/queries/`,
      },
    },
    {
      resolve: '@wyze/gatsby-source-graphql',
      options: {
        headers: {},
        url: `${ process.env.HASURA_GRAPHQL_URL }`,
      },
    },
  ]
};
