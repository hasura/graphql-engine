---
title: "Add Remote Schema"
---

We have written the custom resolver and deployed it to Glitch. We have the GraphQL endpoint ready. Let's add it to Hasura as a remote schema.

## Add

Head to the `Remote Schemas` tab of the console and click on the `Add` button.

![Add remote schema](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/add-remote-schema.png)

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

![remote schema query](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/query-remote-schema.png)

As you can see, Hasura has merged the custom GraphQL schema with the already existing auto-generated APIs over Postgres.