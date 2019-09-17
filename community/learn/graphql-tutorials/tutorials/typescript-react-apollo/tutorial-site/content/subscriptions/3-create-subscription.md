---
title: "Create Subscription and Render Result"
metaTitle: "Apollo useSubscription hook | GraphQL React Apollo Typescript Tutorial"
metaDescription: "Integrate React Apollo Subscription hook to watch for changes in realtime data. We use GraphQL subscriptions as an example to get live data in the React app"
---

import GithubLink from "../../src/GithubLink.js";

So let's define the graphql subscription to be used.

Open `src/components/OnlineUsers/OnlineUsersWrapper.tsx` and add the following code, below the other imports

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/OnlineUsers/OnlineUsersWrapper.tsx" text="src/components/OnlineUsers/OnlineUsersWrapper.tsx" />

```javascript

  import React, { useEffect } from "react";
  import gql from "graphql-tag";
- import { useMutation } from "@apollo/react-hooks";
+ import { useMutation, useSubscription } from "@apollo/react-hooks";

  import OnlineUser from "./OnlineUser";

  const UPDATE_LASTSEEN_MUTATION=gql`
    mutation updateLastSeen ($now: timestamptz!) {
      update_users(where: {}, _set: {last_seen: $now}) {
        affected_rows
      }
    }
  `;

+ const GET_ONLINE_USERS = gql`
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

We are importing the `useSubscription` hook from `@apollo/react-hooks` and the graphql subscription query is defined above to fetch the online user data.

Now, we will pass the subscription constant to `useSubscription` hook.

```javascript
const OnlineUsersWrapper = () => {

  const [updateLastSeen] = useMutation(UPDATE_LASTSEEN_MUTATION);
+ const { data, loading, error } = useSubscription(GET_ONLINE_USERS);
```

Now that we have the real data, let's remove the mock online user state and handle loading/error states.

```javascript

  const OnlineUsersWrapper = () => {

    const [updateLastSeen] = useMutation(UPDATE_LASTSEEN_MUTATION);
    const { data, loading, error } = useSubscription(GET_ONLINE_USERS);

    useEffect(() => {
      const onlineIndicator = setInterval(() => updateLastSeen({variables: { now: (new Date()).toISOString()}}), 20000);
      return () => clearInterval(onlineIndicator);
    });

+   if(loading) {
+     return (<div>Loading...</div>);
+   }
+   if(error || !data) {
+     return(<div>Error...</div>);
+   }

-   const online_users = [
-     { name: "someUser1" },
-     { name: "someUser2" }
-   ];

-   const onlineUsersList = online_users.map((user, index) => (
+   const onlineUsersList = data.online_users.map((user:any, index:number) => (
      <OnlineUser
-       user={user}
+       user={user.user}
        key={index}
      />)
    );

    return (
      <div className="onlineUsersWrapper">
        <div className="sliderHeader">
-         Online users - {online_users.length}
+         Online users - {data.online_users.length}
        </div>
        { onlineUsersList }
      </div>
    );

  }

```

How does this work?
-------------------

We are using the `useSubscription` hook which gives a result object. The `data` key gives the result of the realtime data for the query we have made.

Refresh your react app and see yourself online! Don't be surprised; There could be other users online as well.

Mapping Types
-------------

We need type definitions for the mutation and subscription requests made in this component.

For the mutation, we need the type definitions for `UpdateLastSeenMutation` and for the subscriptions, we need the type definitions for `GetOnlineUsersSubscription`.

```javascript

  import React, { useEffect } from "react";
  import gql from "graphql-tag";
  import { useMutation, useSubscription } from "@apollo/react-hooks";
+ import { UpdateLastSeenMutation, GetOnlineUsersSubscription } from '../../generated/graphql';

```

Let's apply this to the hooks.

```javascript

  const OnlineUsersWrapper = () => {

-   const [updateLastSeen] = useMutation(UPDATE_LASTSEEN_MUTATION);
+   const [updateLastSeen] = useMutation<UpdateLastSeenMutation>(UPDATE_LASTSEEN_MUTATION);
-   const { data, loading, error } = useSubscription(GET_ONLINE_USERS);
+   const { data, loading, error } = useSubscription<GetOnlineUsersSubscription>(GET_ONLINE_USERS);

```

Finally, we will remove the explicit usage of `any` by importing the types of `Online_Users` in both `OnlineUsersWrapper.tsx` and `OnlineUser.tsx` files.

```javascript

- import { UpdateLastSeenMutation, GetOnlineUsersSubscription } from '../../generated/graphql';
+ import { UpdateLastSeenMutation, GetOnlineUsersSubscription, Online_Users } from '../../generated/graphql';
  
  ...
  ...

- const onlineUsersList = data.online_users.map((user:any, index:number) => (
+ const onlineUsersList = data.online_users.map((user:Online_Users, index:number) => (
    <OnlineUser
      user={user.user}
      key={index}
    />)
  );

```

And now open `src/components/OnlineUsers/OnlineUser.tsx`

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/OnlineUsers/OnlineUser.tsx" text="src/components/OnlineUsers/OnlineUser.tsx" />

```javascript

  import * as React from 'react';
+ import { Online_Users } from '../../generated/graphql';

- type user = {
-   name: string;
- }

- const OnlineUser = ({user}:{user: user}) => {
+ const OnlineUser = ({user}: Online_Users) => {
    return (
      <div className="userInfo">
        <div className="userImg">
          <i className="far fa-user" />
        </div>
        <div className="userName">
          {user ? user.name : null}
        </div>
      </div>
    );
  };

  export default OnlineUser;

```

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
