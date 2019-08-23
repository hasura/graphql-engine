---
title: "Create Subscription and Render Result"
metaTitle: "Vue Apollo Subscription Example | GraphQL Vue Apollo Tutorial"
metaDescription: "Integrate Vue Apollo Smart Subscription to watch for changes in realtime data. We use GraphQL subscriptions as an example to get live data in the Vue app"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/E5Is7bshZXM" />

## Add subscription query

So let's define the graphql subscription to be used.

Open `src/components/OnlineUsers.vue` and add the following code, below the other import.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/vue-apollo/app-final/src/components/OnlineUsers.vue" text="src/components/OnlineUsers.vue" />

```javascript
<script>
  import gql from 'graphql-tag'
+ const SUBSCRIPTION_ONLINE_USERS = gql`
+   subscription getOnlineUsers {
+     online_users(order_by: {user: {name: asc }}) {
+       id
+       user {
+         name
+       }
+     }
+   }
+ `;
```

We are defining the graphql subscription query to fetch the online user data. Now let's define a smart subscription.

```javascript
export default {
  data() {
    return {
      online_list: [
        { user: { name: "someUser1" }},
        { user: { name: "someUser2" }}
      ]
    };
  },
  mounted() {
    ...
  },
+ apollo: {
+   // Subscriptions
+   $subscribe: {
+     // When a user is added
+     online_users: {
+       query: SUBSCRIPTION_ONLINE_USERS,
+       // Result hook
+       result (data) {
+         // Let's update the local data
+         this.online_list = data.data.online_users
+       },
+     },
+   },
+ },
}
```

## Remove mock data

Now that we have the real data, let's remove the mock online user state

```javascript
<script>
  import gql from 'graphql-tag'
  const SUBSCRIPTION_ONLINE_USERS = gql`
    subscription getOnlineUsers {
      online_users(order_by: {user: {name: asc }}) {
        id
        user {
          name
        }
      }
    }
  `;
  export default {
    data() {
      return {
        online_list: [
-         { user: { name: "someUser1" }},
-         { user: { name: "someUser2" }}
        ]
      };
    },
```

How does this work?
-------------------

We are using the `apollo` object to define the subscription query, which functions similar to queries. The `online_users` prop gives the result of the realtime data for the query we have made.

Refresh your vue app and see yourself online! Don't be surprised ;) There could be other users online as well.

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscription. Easy isn't it?
