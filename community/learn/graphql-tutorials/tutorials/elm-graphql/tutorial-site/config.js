const config = {
	"gatsby": {
		"pathPrefix": "/graphql/elm-graphql",
		"siteUrl": "https://learn.hasura.io",
		"gaTrackingId": "UA-59768903-1"
	},
	"header": {
		"logo": "https://graphql-engine-cdn.hasura.io/img/hasura_icon_white.svg",
		"title": "/ graphql / elm-graphql",
		"githubUrl": "https://github.com/hasura/graphql-engine",
		"helpUrl": "https://discordapp.com/invite/vBPpJkS",
		"tweetText": "Check out this GraphQL course for Elm developers by @HasuraHQ https://learn.hasura.io/graphql/elm-graphql",
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
    		"/elm-graphql",
    		"/queries",
    		"/mutations-variables",
        "/apollo-client",
    		"/subscriptions",
    		"/realtime-feed",
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
		"title": "2 hour GraphQL course for Elm developers | Hasura",
		"description": "A concise and powerful tutorial that covers fundamental concepts of both GraphQL and using GraphQL in Elm",
		"ogImage": "https://graphql-engine-cdn.hasura.io/learn-hasura/assets/social-media/twitter-card-elm.jpg",
		"docsLocation": "https://github.com/hasura/graphql-engine/tree/master/community/learn/graphql-tutorials/tutorials/elm-graphql/tutorial-site/content",
    "favicon": "https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-elm/favicon.ico"
	},
};

module.exports = config;
