---
title: "Basic Data Modelling"
---

In this part of the course, we will build the data model for a realtime todo app. Our todo app will have the following features:

- Users can maintain personal todos
- Users can view public todos
- A list of current online users using the app
- Send email when a user signups

Broadly this means that we have two models in this app: `users` and `todos`, each with its own set of properties.

We will go over them in the subsequent steps.

The final model looks like the following:

![Schema Todo app](/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/voyager-schema.png)

As we create tables using the console or directly on postgres, Hasura GraphQL engine creates GraphQL schema object types and corresponding query/mutation fields with resolvers automatically.

