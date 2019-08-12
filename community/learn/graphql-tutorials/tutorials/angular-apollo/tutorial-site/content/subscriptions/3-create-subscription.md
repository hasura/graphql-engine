---
title: "Create Subscription and Render Result"
metaTitle: "Apollo Subscription Method | GraphQL Angular Apollo Tutorial"
metaDescription: "Integrate Angular Apollo Subscription Method to watch for changes in realtime data. We use GraphQL subscriptions as an example to get live data in the Angulart app"
---

import GithubLink from "../../src/GithubLink.js";

So let's define the graphql subscription to be used.

Open `src/app/OnlineUsers/OnlineUsersWrapper.ts` and add the following code, below the other imports

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/angular-apollo/app-final/src/app/OnlineUsers/OnlineUsersWrapper.ts" text="src/app/OnlineUsers/OnlineUsersWrapper.ts" />

Now, we will add the `apollo.subscribe` method to get live online users.

```typescript
import { Component, OnInit  } from '@angular/core';
import { Apollo } from 'apollo-angular';
import gql from 'graphql-tag';

import {OnlineUser} from "./OnlineUser"

+ const SUBSCRIBE_TO_ONLINE_USERS = gql`
+ subscription getOnlineUsers {
+  online_users(order_by: {user: {name: asc }}) {
+    id
+    user {
+      name
+    }
+  }
+ }`

@Component({  
    selector: 'OnlineUsersWrapper',  
    templateUrl: './OnlineUsersWrapper.template.html',  
  }) 

export class OnlineUsersWrapper implements OnInit {
    onlineUsers = [
        { name: "someUser1" },
        { name: "someUser2" }
      ];
    onlineIndicator: any;
+    loading: boolean = true;
      
      constructor(private apollo: Apollo) {}

      ngOnInit(){
        this.onlineIndicator = setInterval(() => this.updateLastSeen(), 30000);
+        this.apollo.subscribe({
+          query: SUBSCRIBE_TO_ONLINE_USERS,
+        }).subscribe(({ data, loading }) => {
+          if(data) {
+            const users = data.online_users;
+            this.loading = loading;
+            this.onlineUsers = [];
+              users.forEach((u, index) => {
+                this.onlineUsers.push(u.user)
+              })
+          }
+          console.log('got data ', data);
+        },(error) => {
+          console.log('there was an error sending the query', error);
+        }); 
      }
      updateLastSeen() {
        ...
      }
}
```

Now that we have the real data, let's remove the mock online user state

```typescript
export class OnlineUsersWrapper implements OnInit {
    onlineUsers = [
-        { name: "someUser1" },
-        { name: "someUser2" }
      ];
    onlineIndicator: any;
    loading: boolean = true;

  ...
}      
```

How does this work?
-------------------

We are using the `apollo.subscribe` method  which gives fields (similar to `watchQuery` and `mutate` methods). The `data` field gives the result of the realtime data for the query we have made.

Refresh your angular app and see yourself online! Don't be surprised; There could be other users online as well.

Awesome! You have completed basic implementations of a GraphQL Query, Mutation and Subscriptions. Easy isn't it?
