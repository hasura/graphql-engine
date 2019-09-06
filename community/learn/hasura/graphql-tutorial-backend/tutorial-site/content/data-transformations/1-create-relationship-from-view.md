---
title: "Create relationship to user"
metaTitle: "Create Manual Relationship from View | Hasura GraphQL Tutorial"
metaDescription: "In this part, we learn how to create a manual relationship from the view to the table using the Hasura Console"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/QuaNRk2c5KI" />

Now that the view has been created, we need a way to be able to fetch user information based on the `id` column of the view. Let's create a manual relationship from the view `online_users` to the table `users` using the `id column` of the view.

Head to Console -> Data -> online_users -> Relationships page.

Add new relationship by choosing the relationship type to be `Object Relationship`. Enter the relationship name as `user`.
Select the configuration for current column as `id` and the remote table would be `users` and the remote column would be `id` again.

We are just mapping current view's id column to users table's id column to create the relationship.

![create relationship from view](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/create-relationship-view.png)

Great! We are completely done with data modelling for the app.





