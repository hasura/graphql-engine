---
title: "Custom Business Logic"
metaTitle: "Custom Business Logic | Hasura GraphQL Tutorial"
metaDescription: "Custom business logic can be handled in two ways using Hasura. One is by writing custom GraphQL resolvers and adding it as remote schema and another is to trigger a webhook asynchronously after a mutation."
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/9t-qdjttcyQ" />

Hasura gives you CRUD + realtime GraphQL APIs with authorization & access control. However, there are cases where you would want to add custom/business logic in your app. For example, in the todo app that we are building, before inserting todos into the public feed we want to validate the text for profanity. 

Custom business logic can be handled in two ways using Hasura:
- Writing custom GraphQL resolvers and adding it as a remote schema.
- After a mutation operation, trigger a webhook asynchronously. This can be done via event triggers.

In the todo app backend that you have built, there are certain custom functionalities you may want to add:

- If you want to fetch profile information from Auth0, you need to make an API call to Auth0 with the token. This API has to be exposed to the GraphQL client and hence we will add a custom GraphQL resolver and add it as a remote schema in Hasura.
- Get notified via email whenever a new user registers in your app. This is an asynchronous operation that can be invoked via webhook.

We will see how these 2 use-cases can be handled in Hasura.

