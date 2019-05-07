---
title: "Setup todos table permissions"
---

Head over to the Permissions tab under `todos` table to add relevant permissions.

## Insert permission

- In the enter new role textbox, type in “user”
- click on edit (pencil) icon for “insert” permissions. This would open up a section below which lets you configure custom checks and allow columns.
- In the custom check, choose the following condition
```json
{"user_id":{"_eq":"X-Hasura-User-Id"}}
```

![Todos row permission insert](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/todos-table-row-permission-insert.png)

Now under column insert permissions, select the `title` and `is_public` columns.

![Todos insert column permission](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/todos-insert-column-permission.png)

Click on `Save Permissions`.

## Select permission

Now click on edit icon for "select" permissions. In the custom check, choose the following condition

```json
{"_or":[{"is_public":{"_eq":true}},{"user_id":{"_eq":"X-Hasura-User-Id"}}]}
```

![Todos select permission row](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/todos-select-permission-row.png)

Under column select permissions, select all the columns.

![Todos select column permission](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/todos-select-permission-column.png)

Click on `Save Permissions`

## Update permission

Now click on edit icon for "update" permissions. In the custom check, choose `With same custom checks as insert`.

And under column update permissions, select `id` and `is_completed` columns.

![Todos update permission](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/todos-update-permission.png)

Click on `Save Permissions` once done.

## Delete permission

Finally for delete permission, under custom check, choose `With same custom checks as insert, update`.

![Todos delete permission](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/todos-delete-permission.png)

Click on `Save Permissions` and you are done with access control for `todos` table.