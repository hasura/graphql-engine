---
title: "Connect Hasura with Auth0"
metaTitle: "Connect Hasura with Auth0 | Hasura GraphQL Tutorial"
metaDescription: "In this part, you will learn how to connect Hasura with the Auth0 application and secure your app with HASURA_GRAPHQL_JWT_SECRET and HASURA_GRAPHQL_ADMIN_SECRET"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/86qWv1YU7jA" />

In this part, you will learn how to connect Hasura with the Auth0 application that you just created in the previous step.

We need to configure Hasura to use the Auth0 public keys. An easier way to generate the config for JWT is to use the following link - [https://hasura.io/jwt-config](https://hasura.io/jwt-config)

![jwt-config](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/generate-jwt-config.png)

The generated configuration can be used as the value for environment variable `HASURA_GRAPHQL_JWT_SECRET`. 

Since we have deployed Hasura GraphQL Engine on Heroku, let's head to Heroku dashboard to configure the admin secret and JWT secret.

Open the "Settings" page for your Heroku app, add a new Config Var called `HASURA_GRAPHQL_JWT_SECRET`, and copy and paste the generate JWT configuration into the value box.

Next, create a new Config Var called `HASURA_GRAPHQL_ADMIN_SECRET` and enter a secret key to protect the GraphQL endpoint. (Imagine this as the password to your GraphQL server).

You should end up with something like the following:

![Heroku ENV Config](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/heroku-env-vars.png)

Great! Now your Hasura GraphQL Engine is secured using Auth0.




