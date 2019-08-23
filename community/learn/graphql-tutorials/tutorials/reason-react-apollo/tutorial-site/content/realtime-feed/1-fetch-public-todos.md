---
title: "Fetch public todos"
metaTitle: "Fetch all public todos with pagination | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "You will learn how to make use of Apollo Client instance to make GraphQL queries without the JSX components"
---

In this section, we are going to need to fire queries without using the JSX components. This means, we need access to the Apollo Client instance. Wrap the required component in an ApolloConsumer so that you can access the `client`. Go to `src/todo/TodoPublicWrapper.re` and wrap `TodoPublicList` component with `ApolloConsumer`.

```js
[@react.component]
let make = () => {
  <div className="todoWrapper">
    <div className="sectionHeader">
      {ReasonReact.string("Public feed (realtime)")}
    </div>
    <TodoInput isPublic=true/>
+    <ReasonApollo.Consumer>
+      ...{
+        client => {
+          <TodoPublicList client={client}/>
+        }
+      }
+    </ReasonApollo.Consumer>
-    <TodoPublicList/>
  </div>
}
```

Also change the `TodoPublicList` component to accept this client instance. In `src/todo/TodoPublicList.re`, just add `client` as a prop.

```
-let make = () => {
+let make = (~client) => {
```

Now we can access the client instance from the props of `TodoPublicList`.

For the public todos, we will not use the JSX component by apollo. We will make the query using the client instance and store the tasks in the React state. So let us define the type of the state and the GraphQL response. We also need an external binding `toApolloResult` that parses the GraphQL response according to these types. Go to `src/todo/TodoPublicList.re` and add the following code at the top:

```js
+type user = {
+  .
+  "name": string
+};
+
+type todoType = {
+  .
+  "id": int,
+  "created_at": string,
+  "title": string,
+  "user": user
+};
+
+type state = {
+  .
+  "todos": array(todoType),
+  "loading": bool,
+};
+
+type todosGqlResp = { 
+  .
+  "data": state
+};
+
+type response = {. "data": state};
+
+external toApolloResult : 'a => response = "%identity";
```

Now, let us define the action type of the React reducer:

```js
+type action = SetTodos(array(todoType));

let make = (~client) => {
```

Initialize the state with the `useReducer` hook.

```js
let make = (~client) => {

+  let (state, dispatch) = React.useReducer((_, action) => {
+    let SetTodos(todos) = action;
+    { "todos": todos, "loading": false }
+  }, { "todos": [||], "loading": true });
```

This will initialize the state with `todos` being an empty array while `loading` being true because the component must be in loading state before the todos have been loaded.

Let us now load all the todos in before the first render using the `useEffect` hook.

We will start with fetching the newest 10 public tasks. Lets define the query first. Open `src/GraphQLQueries.re` and add the following code at the bottom:

```js
// graphql query to fetch public todos
module getpublictodos = [%graphql
  {|
     query getpublictodos {
       todos(where: { is_public: { _eq: true } }, order_by: { id: desc }, limit: 10) {
        id
         title
         created_at
         user {
           name
         }
       }
     }
  |}
];
```

Now, write a function in `TodoPublicList` that fetches data using the above query and stores it in the local state.

```js
let fetchPublicTodos = () => {
  let fetchPublicTodosQuery = GraphQLQueries.GetPublicTodos.make(());
  let query = {
    "query": ApolloClient.gql(. fetchPublicTodosQuery##query),
    "variables": fetchPublicTodosQuery##variables
  };
  let apolloData = client##query(query);
  apolloData
  |> Js.Promise.then_(gqlResp => {
        let resp = toApolloResult(gqlResp);
        dispatch(SetTodos(resp##data##todos));
        Js.Promise.resolve()
     })
  |> ignore
};
```

We will call this function in the `useEffect` hook to load the newest 10 public tasks.

```js
React.useEffect0(
  () => {
    fetchPublicTodos();
    None;
  }
);
```

We also have to handle the `loading` state. We can do that by adding an `if` condition.

```js
if (state##loading) {
  <div>
    {ReasonReact.string("Loading...")}
  </div>
} else {
  //Rest of the JSX
}
```

Finally, make the UI render the todos from this state instead of `sampleTodos`.

```
-let todoList = Array.map((t) => {
      <FeedItem todo={t} key={t##title} />
    }, sampleTodos);
+let todoList = Array.map((t) => {
      <FeedItem todo={t} key={t##title} />
    }, state##todos);
```

Once this is done, the UI should render the newest public todos.

Also, we will add basic pagination by implementing the `Load older todos` button.

Define the query to load older tasks in `src/GraphQLQueries.re`

```js
// GraphQL query to public todos older than a particular id
module GetOlderTodos = [%graphql
  {|
    query ($lastId: Int) {
      todos (
        order_by: {
         created_at: desc
        },
        where: { id: { _lt: $lastId} },
        limit: 10
      ) {
        id
        title
        created_at
        user {
          name
        }
      }
    }
  |}
];
```

This query takes the `lastId` as an arguemnt and fetches 10 todos who have `id` less than the provided `lastId`.

Lets go back to `src/todo/TodoPublicList.re` and write a function that looks at the `lastId` from the local state and uses the above query to append the older todos to the array of existing todos.

```js
let fetchOlderTodos = () => {
  let existingTodoLength = Array.length(state##todos);
  let lastTodoId = if (existingTodoLength == 0) { 10000000 } else { state##todos[existingTodoLength-1]##id };
  let fetchOlderTodosQuery = GraphQLQueries.GetOlderTodos.make(~lastId=lastTodoId, ());
  let query = {
    "query": ApolloClient.gql(. fetchOlderTodosQuery##query),
    "variables": fetchOlderTodosQuery##variables
  };
  let apolloData = c##query(query);
  apolloData
  |> Js.Promise.then_(gqlResp => {
    let resp = toApolloResult(gqlResp);
    let newTodos = Array.append(state##todos, resp##data##todos);
    dispatch(SetTodos(newTodos));
    Js.Promise.resolve();
  })
  |> ignore
};
```

And add this function in the `onClick` handler of the `oldTodosButton`

```
    let oldTodosButton = {
-      <div className={"loadMoreSection"}>
+      <div className={"loadMoreSection"} onClick={_ => fetchOlderTodos()}>
        {ReasonReact.string("Load older tasks")}
      </div>
    };

```


With this, you will have a public todo list with basic pagination implemented.
