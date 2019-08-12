---
title: "Rules for Custom JWT Claims"
metaTitle: "Rules for Custom JWT Claims | Hasura GraphQL Tutorial"
metaDescription: "Custom Claims inside the JWT are used to tell Hasura about the role of the caller, so that Hasura may enforce the necessary authorization rules to decide what the caller can and cannot do."
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/AAVn87dBOCU" />

[Custom claims](https://auth0.com/docs/scopes/current/custom-claims) inside the JWT are used to tell Hasura about the role of the caller, so that Hasura may enforce the necessary authorization rules to decide what the caller can and cannot do.
In the Auth0 dashboard, navigate to [Rules](https://manage.auth0.com/#/rules). 

Add the following rule to add our custom JWT claims under `hasura-jwt-claim`:

```javascript
function (user, context, callback) {
  const namespace = "https://hasura.io/jwt/claims";
  context.idToken[namespace] = 
    { 
      'x-hasura-default-role': 'user',
      // do some custom logic to decide allowed roles
      'x-hasura-allowed-roles': ['user'],
      'x-hasura-user-id': user.user_id
    };
  callback(null, user, context);
}
```

![Custom JWT Claims Rule](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/custom-jwt-claims-rule.png)


