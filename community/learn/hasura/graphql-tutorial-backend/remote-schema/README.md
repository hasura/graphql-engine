# auth0-hasura-remote-schema

[![DEPLOY TO GLITCH](https://raw.githubusercontent.com/hasura/graphql-engine/master/community/boilerplates/auth-webhooks/nodejs-express/assets/deploy-glitch.png)](https://glitch.com/~auth0-hasura-remote-schema)

### Environment variables
After remixing to your own project on Glitch, modify the `.env` file to enter the 
- `AUTH0_MANAGEMENT_API_TOKEN`
- `AUTH0_DOMAIN` 

values appropriately.

### Remote schema
1. Schema:

```
type auth0_profile {
  email: String
  picture: String
}
type Query {
  auth0: auth0_profile
}
```

2. Click on **Show** *Live* at the top of the Glitch UI to get the URL.

3. Add as Remote Schema in Hasura GraphQL Engine.

4. Go to GraphiQL tab, and try out `query { auth0 { email picture } }` .

