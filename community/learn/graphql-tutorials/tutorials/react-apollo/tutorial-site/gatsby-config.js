require("dotenv").config();
module.exports = {
  // pathPrefix: process.env.GATSBY_PATH_PREFIX,
  siteMetadata: {
    title: 'React Apollo | GraphQL Tutorial',
    description: 'GraphQL tutorial for react developers using Apollo client',
    docsLocation: 'https://github.com/hasura/graphql-engine/tree/master/community/learn/graphql-tutorials/tutorials/react-apollo/tutorial-site/content',
    headerTitle: 'React Apollo'
  },
  plugins: [
    `gatsby-plugin-sharp`,
    {
      resolve: `gatsby-mdx`,
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
    `gatsby-plugin-emotion`,
    `gatsby-plugin-remove-trailing-slashes`,
    `gatsby-plugin-react-helmet`,
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "docs",
        path: `${__dirname}/content/`
      }
    },
  ]
};
