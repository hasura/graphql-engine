module.exports = {
	"siteMetadata": {
    "title": 'GraphQL Tutorials for frontend developers | learn.hasura.io',
		"description": 'Learn how to integrate GraphQL APIs with React, Apollo and Hasura GraphQL Engine',
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