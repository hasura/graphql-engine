---
title: "Add Remote Schema"
metaTitle: "Add Remote Schema | Hasura GraphQL Tutorial"
metaDescription: "In this part, we will look at how to add a remote schema in Hasura GraphQL Engine using the console"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/01t4t2t4q1c" />

We have written the custom resolver and deployed it to Glitch. We have the GraphQL endpoint ready. Let's add it to Hasura as a remote schema.

## Add

Head to the `Remote Schemas` tab of the console and click on the `Add` button.

![Add remote schema](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/add-remote-schema.png)

Give a name for the remote schema (let's say auth0).
Under GraphQL Server URL, enter the glitch app url that you just deployed in the previous step.

Select `Forward all headers from the client` and click on `Add Remote Schema`.

## Try it out

Head to Console GraphiQL tab and explore the following GraphQL query.

```graphql
query {
  auth0 {
    email
    picture
  }
}
```

You also need to pass in the `Authorization` header with the token to get the right data.

![remote schema query](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/query-remote-schema.png)

As you can see, Hasura has merged the custom GraphQL schema with the already existing auto-generated APIs over Postgres.