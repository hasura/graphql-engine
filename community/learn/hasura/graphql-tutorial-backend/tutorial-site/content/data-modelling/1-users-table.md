---
title: "Create table users"
metaTitle: "Create table users | Hasura GraphQL Tutorial"
metaDescription: "Lets create table users with Hasura console by heading to Data tab and clicking on Create table"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/YLRYrEgJRA0" />

Let's get started by creating the table users.

The `users` table will have the following columns:

- `id` (type text), 
- `name` (type text), 
- `created_at` (type timestamp and default now())
- `last_seen` (type timestamp and nullable)

The columns are mostly self-explanatory. The `last_seen` column is used to store the latest timestamp of when the user was online.

In the Hasura Console, head over to the `Data` tab section and click on `Create Table`. Enter the values for creating the table as mentioned above.

![Create table users](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/create-table-users.png)

Once you are done, click on `Create` button to create the table.

Great! You have created the first table required for the app.