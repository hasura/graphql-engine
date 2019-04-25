require("dotenv").config();
module.exports = {
  pathPrefix: process.env.GATSBY_PATH_PREFIX,
  siteMetadata: {
    title: 'GraphQL for react devs | Hasura',
    description: 'Introduction to GraphQL course react developers',
    docsLocation: 'https://github.com/hasura/graphql-engine/tree/master/community/learn/graphql-tutorials/tutorials/react-apollo/tutorial-site/content',
    headerTitle: '/ graphql / react'
  },
  plugins: [
    'gatsby-plugin-sharp',
    {
      resolve: `gatsby-plugin-layout`,
      options: {
          component: require.resolve(`./src/templates/docs.js`)
      }
    },
    {
      resolve: 'gatsby-mdx',
      options: {
        gatsbyRemarkPlugins: [
          {
            resolve: "gatsby-remark-images",
            options: {
              maxWidth: 1035,
              sizeByPixelDensity: true
            }
          },
          {
            resolve: 'gatsby-remark-copy-linked-files'
          }
        ],
        extensions: [".mdx", ".md"]
      }
    },
    'gatsby-plugin-emotion',
    'gatsby-plugin-remove-trailing-slashes',
    'gatsby-plugin-react-helmet',
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "docs",
        path: `${__dirname}/content/`
      }
    },
    {
      resolve: `gatsby-plugin-gtag`,
      options: {
        // your google analytics tracking id
        trackingId: `UA-59768903-1`,
        // Puts tracking script in the head instead of the body
        head: true,
        // enable ip anonymization
        anonymize: false,
      },
    },
  ]
};
