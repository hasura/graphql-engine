---
title: "Authorization"
metaTitle: "Authorization with Hasura | Hasura GraphQL Tutorial"
metaDescription: "This part of the tutorial covers how to do Authorization in Hasura GraphQL Engine by defining role based access control rules for the models."
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/URMEgibHof0" />

In this part of the tutorial, we are going to define role based access control rules for each of the models that we created.

Access control rules help in restricting querying on a table based on certain conditions.

In this realtime todo app use-case, we need to restrict all querying only for logged in users. Also certain columns in tables do not need to be exposed to the user.

The aim of the app is to allow users to manage their own todos only but should be able to view all the public todos.

We will define all of these based on role based access control rules in the subsequent steps.