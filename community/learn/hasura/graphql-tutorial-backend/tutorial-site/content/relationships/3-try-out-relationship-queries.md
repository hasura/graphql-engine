---
title: "Try out Relationship Queries"
metaTitle: "Try out Relationship Queries | Hasura GraphQL Tutorial"
metaDescription: "Explore GraphQL APIs for the table todos with simple queries and nested data using Hasura GraphQL Engine"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/0-UZahHSoGg" />

Let's explore the GraphQL APIs for the relationship created.

```graphql
query {
  todos {
    id
    title
    user {
      id
      name
    }
  }
}
```

You can see the response in the following format:

![relationship query](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/graphiql-relationship-query.png)

As you can see, in the same response, you are getting the results for the user's information, exactly like you queried. This is a simple example of a one-to-one query/object relationship.