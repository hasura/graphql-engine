---
title: "Setup online_users view permissions"
---

Head over to the Permissions tab under `online_users` view to add relevant permissions. 

## Select permission

Here in this view, we only want the user to be able to select data and not do any mutations. Hence we don't define any permission for insert, update or delete.

For Row select permission, choose `Without any checks` and under Column select permission, choose both the columns `id` and `last_seen`.

![online users permission](https://graphql-engine-cdn.hasura.io/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/online-users-permission.png)

Click on `Save Permissions`. You have completed all access control rules required for the realtime todo app.

