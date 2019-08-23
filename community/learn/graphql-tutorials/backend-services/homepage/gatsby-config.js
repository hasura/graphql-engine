module.exports = {
	"siteMetadata": {
    "title": 'GraphQL Tutorials for frontend developers | learn.hasura.io',
		"description": 'Learn how to integrate GraphQL APIs with React, Apollo and Hasura GraphQL Engine',
		"siteUrl": 'https://learn.hasura.io'
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
				policy: [{ userAgent: '*', allow: '/' }],
				sitemap: null,
				host: null
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