module.exports = {
	"siteMetadata": {
    "title": 'GraphQL Tutorials | learn.hasura.io',
		"description": 'GraphQL tutorials using Hasura GraphQL Engine'
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
				policy: [{ userAgent: '*', disallow: ['/'] }]
			}
		}
	]
};