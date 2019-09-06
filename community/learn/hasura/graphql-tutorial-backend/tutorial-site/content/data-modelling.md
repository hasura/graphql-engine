---
title: "Basic Data Modelling"
metaTitle: "Basic Data Modelling with Hasura | Hasura GraphQL Tutorial"
metaDescription: "This tutorial covers how to do basic data modelling in Postgres and create tables using Hasura console"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/XURdIpvfp4M" />

In this part of the course, we will build the data model for a realtime todo app. Our todo app will have the following features:

- Users can maintain personal todos
- Users can view public todos
- A list of current online users using the app
- Send email when a user signs up

Broadly this means that we have two models in this app: `users` and `todos`, each with its own set of properties.

We will go over them in the subsequent steps.

The final model looks like the following:

![Schema Todo app](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/voyager-schema.png)

As we create tables using the console or directly on postgres, Hasura GraphQL engine creates GraphQL schema object types and corresponding query/mutation fields with resolvers automatically.

