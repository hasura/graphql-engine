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
		"tweetText": "Check out this GraphQL backend course for frontend developers by @HasuraHQ https://learn.hasura.io/graphql/hasura",
		"links": [{
			"text": "hasura.io",
			"link": "https://hasura.io"
		}],
	},
	"sidebar": {
		"forcedNavOrder": [
			"/introduction",
    		"/intro-to-graphql",
    		"/setup",
    		"/data-modelling",
    		"/relationships",
    		"/authorization",
    		"/authentication",
    		"/data-transformations",
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
		"title": "30 mins Hasura course for frontend developers | Hasura",
		"description": "A concise and powerful tutorial that covers fundamental concepts of both GraphQL and developing GraphQL backends instantly using Hasura",
		"ogImage": "https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/twitter-card-hasura.png",
		"docsLocation": "https://github.com/hasura/graphql-engine/tree/master/community/learn/hasura/graphql-tutorials-backend/tutorial-site/content",
		"favicon": "https://graphql-engine-cdn.hasura.io/img/hasura_icon_black.svg"
	},
};

module.exports = config;
