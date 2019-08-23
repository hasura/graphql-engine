---
title: 'Logging out'
---


import GithubLink from "../src/GithubLink.js";

Remember to clear the Apollo cache before logging out. You can use the instance of Apollo client to do so.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/LogoutScreen.js" text="LogoutScreen.js"/>

```js
client.resetStore();
```