---
title: "Relationships"
metaTitle: "Relationships with Hasura | Hasura GraphQL Tutorial"
metaDescription: "This part of the tutorial covers how to make nested object queries by using object relationships and array relationships"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/82t_AjcAtdc" />

Relationships enable you to make nested object queries if the tables/views in your database are connected. 

GraphQL schema relationships can be either of

- object relationships (one-to-one)
- array relationships (one-to-many)

## Object Relationships

Let's say you want to query `todos` and more information about the `user` who created it. This is achievable using nested queries if a relationship exists between the two. This is a one-to-one query and hence called an object relationship.

An example of such a nested query looks like this:

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

In a single query, you are able to fetch todos and its related user information. This can be very powerful because you can nest to any level.

## Array Relationships

Let's look at an example query for array relationships.

```graphql
query {
  users {
    id
    name
    todos {
      id
      title
    }
  }
}
```

In this query, you are able to fetch users and for each user, you are fetching the todos (multiple) written by that user. Since a user can have multiple todos, this would be an array relationship.

Relationships can be captured by foreign key constraints. Foreign key constraints ensure that there are no dangling data.
Hasura Console automatically suggests relationships based on these constraints.

Though the constraints are optional, it is recommended to enforce these constraints for data consistency.

The above queries won't work yet because we haven't defined the relationships yet. But this gives an idea of how nested queries work.

