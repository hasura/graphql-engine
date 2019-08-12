---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL Angular Apollo Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

Once a new todo is entered in a public list, it needs to appear in the UI. Instead of automatically displaying the todo in the UI, we use a Feed like Notification banner which appears whenever a new todo is received.

Remember that previously we updated the cache using the cache API and the UI got updated automatically, because updating the cache triggered a re-render for those components that were subscribed to this store.

We are not going to use that approach here since we don't want public list UI to be automatically updated.

In the Subscription method of the previous step, we only get the latest todo and not the existing list. We will now write a simple query to fetch the list of existing public todos.

```typescript
export class TodoPublicList implements OnInit {

     olderTodosAvailable= true;
     newTodosCount = 0;
     oldestTodoId;
     newestTodoId;
      todos= [
-       {
-         id: "1",
-         title: "This is public todo 1",
-         user: {
-           name: "someUser1"
-         }
-       },
-       {
-         id: "2",
-         title: "This is public todo 2",
-         is_completed: false,
-         is_public: true,
-         user: {
-           name: "someUser2"
-         }
-       },
-       {
-         id: "3",
-         title: "This is public todo 3",
-         user: {
-           name: "someUser3"
-         }
-       },
-       {
-         id: "4",
-         title: "This is public todo 4",
-         user: {
-           name: "someUser4"
-         }
-       }
      ];

  ...

}
```

Update the `loadOlder` method to the following:

```typescript
  loadOlder() {
+    const GET_OLD_PUBLIC_TODOS = gql`
+      query getOldPublicTodos ($oldestTodoId: Int!) {
+        todos (where: { is_public: { _eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: { created_at: desc }) {
+          id
+          title
+          created_at
+          user {
+            name
+          }
+        }
+      }`;
+    this.apollo.watchQuery({
+      query: GET_OLD_PUBLIC_TODOS,
+      variables: {oldestTodoId: this.oldestTodoId}
+    })
+    .valueChanges
+    .subscribe(({ data, loading }) => {
+      const todosData : any = data;
+      if(todosData) {
+        if (todosData.todos.length) {
+          this.oldestTodoId = todosData.todos[todosData.todos.length - 1].id;
+          this.todos = [...this.todos, ...todosData.todos]
+        } else {
+          this.olderTodosAvailable = false;
+        }
+      }
+      console.log('got data', data);
+    },(error) => {
+      console.log('there was an error sending the query', error);
+    });
}
```

We are defining a query to fetch older public todos and making a `apollo.watchQuery` call to get the data from the database. Once we get the data, we update the `todos` state to re-render the UI with the available list of public todos.

Now try adding a new todo to the public feed and you will see the notification appearing saying that a new task has arrived.

Great! We still have one functionality left. When a new task arrives on the public feed and when the user clicks on the New tasks section, we should make a query to re-fetch the todos that are not present on our current public feed.

Update `loadNew()` method with the following code

```typescript
  loadNew() {
+  const GET_NEW_PUBLIC_TODOS = gql`
+        query getNewPublicTodos ($latestVisibleId: Int!) {
+          todos(where: { is_public: { _eq: true}, id: {_gt: $latestVisibleId}}, order_by: { created_at: desc }) {
+            id
+            title
+            created_at
+            user {
+              name
+            }
+          }
+        }
+      `;
+      this.apollo.watchQuery({
+        query: GET_NEW_PUBLIC_TODOS,
+        variables: {latestVisibleId: this.todos[0].id}
+      })
+      .valueChanges
+      .subscribe(({ data, loading }) => {
+        const todosData : any = data;
+        if(todosData) {
+          this.newestTodoId = todosData.todos[0].id;
+          this.todos = [...todosData.todos, ...this.todos]
+          this.newTodosCount=0;
+        }
+        console.log('got data', data);
+      },(error) => {
+        console.log('there was an error sending the query', error);
+      });
+  }
```