---
title: "Setup online_users view permissions"
metaTitle: "Setup online_users view permissions | Hasura GraphQL Tutorial"
metaDescription: "This tutorial covers how to set up permissions for online_users view for select operation using Hasura console"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/mmX5JRhT1-c" />

Head over to the Permissions tab under `online_users` view to add relevant permissions. 

## Select permission

Here in this view, we only want the user to be able to select data and not do any mutations. Hence we don't define any permission for insert, update or delete.

For Row select permission, choose `Without any checks` and under Column select permission, choose both the columns `id` and `last_seen`.

![online users permission](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/online-users-permission.png)

Click on `Save Permissions`. You have completed all access control rules required for the realtime todo app.

