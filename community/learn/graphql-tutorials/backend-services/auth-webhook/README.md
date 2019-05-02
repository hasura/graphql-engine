# Auth Webhook for learn.hasura.io

This is an auth webhook for authenticating requests to the Hasura GraphQL engine on learn.hasura.io. Splits between Auth0 and Custom JWT.

## Usage with Hasura GraphQL engine

Once you have deployed this webhook, you can use it along with the GraphQL engine. You have to set the webhook URL as an environment variable in the docker container that runs the GraphQL engine.

*[Read the docs](https://docs.hasura.io/1.0/graphql/manual/auth/webhook.html).*
