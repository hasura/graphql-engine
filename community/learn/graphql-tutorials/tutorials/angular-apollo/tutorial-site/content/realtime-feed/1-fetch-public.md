---
title: "Fetch public todos - subscription"
metaTitle: "Fetch public todos using Subscription | GraphQL Angular Apollo Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in Angular app"
---

import GithubLink from "../../src/GithubLink.js";

Let's define the graphql query to be used:

Open `src/app/Todo/TodoPublicList.ts` and add the following imports.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/angular-apollo/app-final/src/app/Todo/TodoPublicList.ts" text="src/app/Todo/TodoPublicList.ts" />

```typescript
import { Component, OnInit, Input } from '@angular/core';
+ import gql from 'graphql-tag';

```

Now let's define the subscription query to get notified about new public todos

```typescript
import { Component, OnInit, Input } from '@angular/core';
import gql from 'graphql-tag';

+ // Run a subscription to get the latest public todo
+ const NOTIFY_NEW_PUBLIC_TODOS = gql`
+  subscription notifyNewPublicTodos {
+    todos (where: { is_public: { _eq: true}}, limit: 1, order_by: {created_at: desc }) {
+      id
+      created_at
+    }
+  }
+ `;

```

`Apollo` is being imported from `apollo-angular`

```typescript
import { Component, OnInit, Input } from '@angular/core';
+ import { Apollo } from 'apollo-angular'
import gql from 'graphql-tag';
```

What does the Subscription do?
-----------------------------

The query fetches `todos` with a simple condition; `is_public` must be true. We also limit the number of todos to 1, since we would just like to get notified whenever a new todo comes in.
We sort the todos by its latest created_at time according to the schema. We specify which fields we need for the todos node.

Now, we will add the `getNotifications` method to get notified of a new public todo.

```typescript
export class TodoPublicList implements OnInit {
          olderTodosAvailable= true;
          newTodosCount = 0;
+         oldestTodoId;
+         newestTodoId;
          todos= [
        {
          id: "1",
          title: "This is public todo 1",
          user: {
            name: "someUser1"
          }
        },
        {
          id: "2",
          title: "This is public todo 2",
          is_completed: false,
          is_public: true,
          user: {
            name: "someUser2"
          }
        },
        {
          id: "3",
          title: "This is public todo 3",
          user: {
            name: "someUser3"
          }
        },
        {
          id: "4",
          title: "This is public todo 4",
          user: {
            name: "someUser4"
          }
        }
      ];
+      loading: boolean = true;

+ constructor(private apollo: Apollo) {}

+      ngOnInit() {
+        this.getNotifications();
+      }

+      getNotifications() {
+        this.apollo.subscribe({
+          query: NOTIFY_NEW_PUBLIC_TODOS,
+        }).subscribe(({ data, loading }) => {
+          this.loading = loading;
+          if(data) {
+            const latestTodo = data.todos.length ? data.todos[0] : null;
+            this.olderTodosAvailable = latestTodo? true: false;
+            this.oldestTodoId=latestTodo? (latestTodo.id +1) : 0 ;
+            if (latestTodo && latestTodo.id > this.newestTodoId) {
+              this.newestTodoId = latestTodo.id;
+              this.newTodosCount = this.newTodosCount +1;
+            } else {
+              this.newestTodoId=latestTodo? latestTodo.id : 0;
+              this.loadOlder();
+            }
+          }
+          console.log('got data', data);
+        },(error) => {
+          console.log('there was an error sending the query', error);
+        });
+      }
}
```
