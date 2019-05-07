---
title: "Custom Business Logic"
---

Hasura gives you CRUD + realtime GraphQL APIs with authorization & access control. However there are cases where you would want to query something which is not in your database.

Custom business logic can be handled in two ways using Hasura:
- Writing custom GraphQL resolvers and adding it as a remote schema.
- Creating event triggers that capture events happening on the specified tables.

In the todo app backend that you have built, there are use-cases to query data which is not in the database. For example, 

- If you want to fetch profile information from Auth0, you need to make an API call to Auth0 with the token. 
- Get notified via email whenever a new user registers in your app.

We will see how these 2 use-cases can be handled in Hasura.

