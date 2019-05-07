---
title: "Setup users table permissions"
---

We also need to allow select and update operations into `users` table. 
On the left sidebar, click on the `users` table to navigate to the users table page and switch to the Permissions tab.

## Select permission

Click on the Edit icon (pencil icon) to modify the select permission for role user. This would open up a section below which lets you configure its permissions.

Here the users should be able to access every other user's `id` and `name` data.

![users select permission](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/users-select-permission.png)

Click on `Save Permissions`

## Update permission

The user who is logged in should be able to modify only his own record. So let’s set that permission now. 

In the Row update permission, under custom check, choose the following condition.

```json
{"id":{"_eq":"X-Hasura-User-Id"}}
```

Under column update permissions, select `last_seen` column, as this will be updated from the frontend app.

![users update permission](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/users-update-permission.png)

Click on `Save Permissions` and you are done with access control rules for `users` table.



