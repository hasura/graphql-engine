---
title: "Authentication"
metaTitle: "Authentication with Hasura | Hasura GraphQL Tutorial"
metaDescription: "This part of the tutorial covers how to do Authentication in Hasura GraphQL Engine by integrating with an Authentication provider like Auth0"
---

In this part, we will look at how to integrate an Authentication provider.

The realtime todo app needs to be protected by a login interface. We are going to use [Auth0](https://auth0.com) as the identity/authentication provider for this example.

**Note**: Auth0 has a free plan for upto 7000 active users.

The basic idea is that, whenever a user authenticates with Auth0, the client app receives a token which can be sent in the `Authorization` headers of all GraphQL requests. Hasura GraphQL Engine would verify if the token is valid and allow the user to perform appropriate queries.

Let's get started!