require("dotenv").config()
module.exports = {
  siteMetadata: {
    title: process.env.GATSBY_SITE_TITLE,
    description: process.env.GATSBY_SITE_DESCRIPTION,
    docsLocation: process.env.GATSBY_DOCS_LOCATION,
    pathPrefix: process.env.GATSBY_PATH_PREFIX,
    headerTitle: process.env.GATSBY_HEADER_TITLE,
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
