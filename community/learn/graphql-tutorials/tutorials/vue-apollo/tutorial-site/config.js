const config = {
	"gatsby": {
		"pathPrefix": "/graphql/vue",
		"siteUrl": "https://learn.hasura.io",
		"gaTrackingId": "UA-59768903-1"
	},
	"header": {
		"logo": "https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/logo.png",
		"logoLink": "https://learn.hasura.io",
		"title": "<a href='https://learn.hasura.io'>/ graphql </a><a href='https://learn.hasura.io/graphql/vue'>/ vue</a>",
		"githubUrl": "https://github.com/hasura/graphql-engine",
		"helpUrl": "https://discordapp.com/invite/vBPpJkS",
		"tweetText": "Check out this GraphQL course for Vue developers by @HasuraHQ https://learn.hasura.io/graphql/vue",
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
    		"/apollo-client",
    		"/queries",
    		"/mutations-variables",
    		"/optimistic-update-mutations",
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
		],
		"frontline": false,
		"ignoreIndex": true
	},
	"siteMetadata": {
		"title": "2 hour GraphQL course for Vue developers | Hasura",
		"description": "A concise and powerful tutorial that covers fundamental concepts of both GraphQL and using GraphQL in Vue",
		"ogImage": "https://graphql-engine-cdn.hasura.io/learn-hasura/assets/social-media/twitter-card-vue.png",
		"docsLocation": "https://github.com/hasura/graphql-engine/tree/master/community/learn/graphql-tutorials/tutorials/vue-apollo/tutorial-site/content",
		"favicon": "https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-vue/vue-favicon.png"
	},
};

module.exports = config;
