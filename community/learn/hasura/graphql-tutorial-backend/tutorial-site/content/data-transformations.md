---
title: "Data Transformations"
metaTitle: "Data Transformations in Postgres | Hasura GraphQL Tutorial"
metaDescription: "We are going to leverage Postgres data transformations using Views and SQL Functions to find online users required for the app"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/vunIhyeTaac" />

One of the realtime features of the todo app is to display the list of online users. We need a way to fetch this information based on the value of `last_seen` which tells when the user was last online.

So far we were building tables and relationships. 
Postgres allows you to perform data transformations using:
- Views
- SQL Functions

In this example, we are going to make use of `Views`. This view is required by the app to find the users who have logged in and are online in the last 30 seconds.

## Create View

The SQL statement for creating this view looks like this:

```sql
CREATE OR REPLACE VIEW "public"."online_users" AS 
 SELECT users.id,
    users.last_seen
   FROM users
  WHERE (users.last_seen >= (now() - '00:00:30'::interval));
```

Let's add this view and track the view with Hasura to be able to query it.

Head to Console -> Data -> SQL page.

![Create view online_users](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/create-view.png)

Click on `Run` to create the view.

## Subscription to Online Users

Now let's test by making a subscription query to the `online_users` view.

```graphql
subscription {
  online_users {
    id
    last_seen
  }
}
```

In another tab, update an existing user's `last_seen` value to see the subscription response getting updated.

![Update users last_seen](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/update-users-last-seen.png)

Enter the value as `now()` for the `last_seen` column and click on `Save`.

Now switch back to the tab where your subscription query is running to see the updated response.

![Subscription online users](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/online-users-subscription.png)





