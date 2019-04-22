module.exports = {
	"siteMetadata": {
    "title": 'GraphQL Tutorials | learn.hasura.io',
		"description": 'GraphQL tutorials using Hasura GraphQL Engine',
		"siteUrl": 'http://learn.hasura.io'
  },
	"plugins" : [
		"gatsby-plugin-sass",
		"gatsby-plugin-react-helmet",
		{
				"resolve": "gatsby-plugin-favicon",
				"options": {
					"logo": "./src/favicon.png",
				}
		},
		{
			resolve: 'gatsby-plugin-robots-txt',
			options: {
				policy: [{ userAgent: '*', disallow: ['/'] }],
				sitemap: null,
				host: null
			}
		}
	]
};