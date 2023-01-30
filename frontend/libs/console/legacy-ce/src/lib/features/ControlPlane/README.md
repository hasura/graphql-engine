To generate GraphQL types:

1. Generate the JSON GraphQL schema of lux data service and place it in this directory as `schema.json`. _Note that it is added to .gitignore._

- If you use [GraphQURL](https://github.com/hasura/graphqurl), you can run the following command to generate the schema: `gq http://data.lux-dev.hasura.me/v1/graphql -H 'x-hasura-admin-secret:randomsecret' --introspect --format=json > schema.json`

2. Run `npm run generate-control-plane-gql-types` from the `/console` directory
