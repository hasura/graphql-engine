const config = {
	"gatsby": {
		"pathPrefix": "/graphql/hasura",
		"siteUrl": "https://learn.hasura.io",
		"gaTrackingId": "UA-59768903-1"
	},
	"header": {
		"logo": "https://graphql-engine-cdn.hasura.io/img/hasura_icon_white.svg",
		"title": "/ graphql / hasura",
		"githubUrl": "https://github.com/hasura/graphql-engine",
		"helpUrl": "https://discordapp.com/invite/vBPpJkS",
		"tweetText": "Check out this Introduction to Hasura GraphQL backend course for frontend developers by @HasuraHQ https://learn.hasura.io/graphql/hasura",
		"links": [{
			"text": "hasura.io",
			"link": "https://hasura.io"
		}],
	},
	"sidebar": {
		"forcedNavOrder": [
			"/introduction",
    		"/setup",
    		"/data-modelling",
    		"/relationships",
    		"/data-transformations",
    		"/authorization",
    		"/authentication",
    		"/custom-business-logic",
    		"/what-next"
    		],
		"links": [
			{
			"text": "Hasura Docs",
			"link": "https://docs.hasura.io"
			},
			{
			"text": "GraphQL Docs",
			"link": "https://graphql.org/learn"
			}
		]
	},
	"siteMetadata": {
		"title": "Introduction to Hasura backend course for frontend developers | Hasura",
		"description": "A concise and powerful tutorial that covers fundamental concepts of developing GraphQL backends instantly using Hasura",
		"ogImage": "https://graphql-engine-cdn.hasura.io/learn-hasura/assets/social-media/twitter-card-hasura.png",
		"docsLocation": "https://github.com/hasura/graphql-engine/tree/master/community/learn/hasura/graphql-tutorial-backend/tutorial-site/content",
		"favicon": "https://graphql-engine-cdn.hasura.io/img/hasura_icon_black.svg"
	},
};

module.exports = config;
