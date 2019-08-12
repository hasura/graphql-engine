---
title: "Write custom resolvers"
metaTitle: "Write custom resolvers | Hasura GraphQL Tutorial"
metaDescription: "In this part, we will look at how to write custom resolvers and add it as a Remote schema in Hasura GraphQL Engine."
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/_d3sn_S6u-E" />

Let's take the first use-case of fetching profile information from Auth0.

Ideally you would want to maintain a single GraphQL endpoint for all your data requirements.

Hasura has the ability to merge remote GraphQL schemas and provide a unified GraphQL API. To handle the use-case of fetching Auth0 profile information, we will write custom resolvers in a custom GraphQL server. Hasura can then merge this custom GraphQL server with the existing auto-generated schema. This custom GraphQL server is the `Remote Schema`.

![Remote schema architecture](https://docs.hasura.io/1.0/_images/remote-schemas-arch1.png)

## Write GraphQL custom resolver
So let's write a custom resolver which can be later merged into Hasura's GraphQL API.

```javascript
const { ApolloServer } = require('apollo-server');
const gql = require('graphql-tag');
const jwt = require('jsonwebtoken');
const fetch = require('node-fetch');

const typeDefs = gql`
  type auth0_profile {
      email: String
      picture: String
    }

    type Query {
      auth0: auth0_profile
    }
`;

function getProfileInfo(user_id){
    const headers = {'Authorization': 'Bearer '+process.env.AUTH0_MANAGEMENT_API_TOKEN};
    console.log(headers);
    return fetch('https://' + process.env.AUTH0_DOMAIN + '/api/v2/users/'+user_id,{ headers: headers})
        .then(response => response.json())
}


const resolvers = {
    Query: {
        auth0: (parent, args, context) => {
          // read the authorization header sent from the client
          const authHeaders = context.headers.authorization;
          const token = authHeaders.replace('Bearer ', '');
          // decode the token to find the user_id
          try {
            const decoded = jwt.decode(token);
            const user_id = decoded.sub;
            // make a rest api call to auth0
            return getProfileInfo(user_id).then( function(resp) {
              console.log(resp);
              if (!resp) {
                return null;
              }
              return {email: resp.email, picture: resp.picture};
            });
          } catch(e) {
            console.log(e);
            return null;
          }
        }
    },
};

const context = ({req}) => {
  return {headers: req.headers};
};

const schema = new ApolloServer({ typeDefs, resolvers, context});

schema.listen({ port: process.env.PORT}).then(({ url }) => {
    console.log(`schema ready at ${url}`);
});

```

In the server above, let's breakdown what's happening:

- We define the GraphQL types for `auth0_profile` and `Query`. 
- And then we write a custom resolver for Query type `auth0`, where we parse the `Authorization` headers to get the token. 
- We then decode the token using the `jsonwebtoken` library's `jwt` method. This gives the user_id required to fetch auth0 profile information.
- We make a request to the [Auth0's Management API](https://auth0.com/docs/api/management/v2/create-m2m-app), passing in the token and the user_id to get details about this user.
- Once we get a response, we return back the object `{email: resp.email, picture: resp.picture}` as response. Else, we return `null`.

## Deploy 

[![DEPLOY TO GLITCH](https://raw.githubusercontent.com/hasura/graphql-engine/master/community/boilerplates/auth-webhooks/nodejs-express/assets/deploy-glitch.png)](https://glitch.com/~auth0-hasura-remote-schema)

### Environment variables
After remixing to your own project on Glitch, modify the `.env` file to enter the 
- `AUTH0_MANAGEMENT_API_TOKEN`
- `AUTH0_DOMAIN` 

values appropriately.

Congrats! You have written and deployed your first GraphQL custom resolver.
