---
title: "Sync Users with Rules"
metaTitle: "Sync Auth0 Users with Rules | Hasura GraphQL Tutorial"
metaDescription: "In this part, you will learn to set up a rule in Auth0 which allows the users of Auth0 to be in sync with the users in our database"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/i5rMmXXcVsk" />

We need to set up a rule in Auth0 which allows the users of Auth0 to be in sync with the users in our database. The following code snippet allows us to do the same. Again using the Rules feature, create a new blank rule and paste in the following code snippet:

```javascript
function (user, context, callback) {
  const userId = user.user_id;
  const nickname = user.nickname;
  
  const admin_secret = "xxxx";
  const url = "https://learn-hasura-backend.herokuapp.com/v1/graphql";

  request.post({
      headers: {'content-type' : 'application/json', 'x-hasura-admin-secret': admin_secret},
      url:   url,
      body:    `{\"query\":\"mutation($userId: String!, $nickname: String) {\\n          insert_users(\\n            objects: [{ id: $userId, name: $nickname }]\\n            on_conflict: {\\n              constraint: users_pkey\\n              update_columns: [last_seen, name]\\n            }\\n          ) {\\n            affected_rows\\n          }\\n        }\",\"variables\":{\"userId\":\"${userId}\",\"nickname\":\"${nickname}\"}}`
  }, function(error, response, body){
       console.log(body);
       callback(null, user, context);
  });
}
```

![Auth0 insert rule](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/create-auth0-insert-rule.png)

**Note**: Modify `x-hasura-admin-secret` and `url` parameters appropriately according to your app.
Here we are making a simple request to make a mutation into `users` table.

That’s it! This rule will now be triggered on every successful signup or login, and we insert or  update the user data into our database using a Hasura GraphQL mutation.

The above request performs a mutation on the users table with the `id` and `name` values.






