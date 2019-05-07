---
title: "Create table todos"
---

Now let's move on to creating the other model `todos`

The `todos` table will have the following columns:

- `id` (type integer;auto-increment), 
- `title` (type text), 
- `is_completed` (type boolean and default false)
- `is_public` (type boolean and default false)
- `created_at` (type timestamp and default now())
- `user_id` (type text) 

The columns are mostly self-explanatory. The `last_seen` column is used to store the latest timestamp of when the user was online.

In the Hasura Console, head over to the `Data` tab section and click on `Create Table`. Enter the values for creating the table as mentioned above.

![Create table users](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/create-table-todos.png)

Once you are done, click on `Create` button to create the table.