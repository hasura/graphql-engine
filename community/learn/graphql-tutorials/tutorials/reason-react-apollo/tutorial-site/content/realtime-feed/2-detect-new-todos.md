---
title: "Detect new todos"
metaTitle: "Subscribe to new todos | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in your app"
---

We will use GraphQL subscriptions to get notified if there are any new public tasks added. Whenever, a notification is received, we will fetch the tasks newer than the newest task in local state.

We wish to show the `newTodosBanner` saying `New tasks have arrived` only when a notification has been received.

We will wrap the parent component `src/todo/TodoPublicWrapper.re` with a `Subscription` and pass the the `newestId` in the database as a prop to the `TodoPublicList` component. Looking at this `newestId`, we will check if it is greater than the `id` of the newest visible todo. If it is, we know that a new task(s) are present in the database and we show the banner saying `New tasks have arrived`.

Lets first define the subscription in `src/GraphQLQueries.re`. Add the following code to the bottom.

```js
module NotifyNewPublicTodos = [%graphql
  {|
    subscription notifyNewPublicTodos {
      todos (where: { is_public: { _eq: true}}, limit: 1, order_by: {id: desc }) {
        id
      }
    }
  |}
];

module NotifyNewPublicTodosSubscription = ReasonApollo.CreateSubscription(NotifyNewPublicTodos);
```

Now, use this `NotifyNewPublicTodosSubscription` component in `TodoPublicWrapper` to wrap `TodoPublicList`. Go to `src/todo/TodoPublicWrapper` and make the following changes:

```js
    <ReasonApollo.Consumer>
      ...{
        client => {
-          <TodoPublicList client={client}/>
+          <GraphQLQueries.NotifyNewPublicTodosSubscription>
+            ...{
+              ({result}) => switch(result) {
+                | Error(error) => {
+                  Js.Console.error(error);
+                  <div> {ReasonReact.string("Error")}</div>
+                }
+                | Data(data) => {
+                  let todos = data##todos;
+                  let latestTodoId = if (Array.length(todos) > 0) { todos[0]##id } else { 0 };
+                  <TodoPublicList client={client} latestTodoId={latestTodoId}/>
+                }
+                | Loading => {
+                  <TodoPublicList client={client} latestTodoId={0}/>
+                }
+              }
+            }
+          </GraphQLQueries.NotifyNewPublicTodosSubscription>
        }
      }
    </ReasonApollo.Consumer>
```

We are making the subscription for the newest task in the database and pass the newest id as a prop called `latestTodoId`.

Now, in `src/todo/TodoPublicList.re`, make the `make` function accept `latestTodoId` as a prop.

```js
-let make = (~client) => {
+let make = (~client, ~latestTodoId) => {
```

Now, compute a boolean variable called `shouldShowNewTodosBanner` which decides if there are new todos in the database. Based on `shouldShowNewTodosBanner`, show or hide the `newTodosBanner`.

```
-    <div className={"loadMoreSection"}>
-      {ReasonReact.string("New tasks have arrived!")}
-    </div>

+  let existingTodoLength = Array.length(state##todos);
+  let latestVisibleId = if (existingTodoLength > 0) { state##todos[0]##id } else { 0 };
+  let shouldShowNewTodosBanner = latestVisibleId < latestTodoId;

+  let newTodosBanner = if(shouldShowNewTodosBanner) {
+    <div className={"loadMoreSection"}>
+      {ReasonReact.string("New tasks have arrived!")}
+    </div>
+  } else {
+    {ReasonReact.null}
+  };
```

Once you do this, whenever there is a new public task in the database, the new todos banner would be displayed. Try entering a new public todo; your own todo will also be treated as a new todo in the database and the banner would be displayed.

In the next section, we will implement the `onClick` handler of this banner i.e we will sync the new todos in the database with the local todos.