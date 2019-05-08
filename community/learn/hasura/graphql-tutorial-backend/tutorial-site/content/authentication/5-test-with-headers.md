---
title: "Test with Auth0 Token"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/i5rMmXXcVsk" />

Hasura is configured to be used with Auth0. Now let's test this setup by getting the token from Auth0 and making GraphQL queries with the Authorization headers to see if the permissions are applied.

To get a JWT token,

1. Login to Auth0 using this URL - https://<auth0-domain>.auth0.com/login?client=<client_id>&protocol=oauth2&response_type=token%20id_token&redirect_uri=<callback_uri>&scope=openid%20profile

- Replace <auth0-domain> with the one we created in the previous steps.
- Replace client_id with Auth0 application's client_id.
- Replace callback_uri with `http://localhost:3000/callback` for testing.

2. After successfully logging in, you will be redirected to https://localhost:3000/callback#xxxxxxxx&id_token=yyyyyyy. This page may be a 404 if you don’t have a UI running on localhost:3000.

3. Extract the id_token value from this URL. This is the JWT.

![jwt-token-auth0-url](https://graphql-engine-cdn.hasura.io/img/id_token-jwt-url.png)

4. Test this JWT in [jwt.io](https://jwt.io) debugger.
