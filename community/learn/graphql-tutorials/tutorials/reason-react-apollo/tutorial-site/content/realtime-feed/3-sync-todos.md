---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

In the previous section, we detected if there are new todos in the database and showed a banner saying `New todos have arrived`. Now, we need to write an onClick handler of that banner that would fetch the todos newer than the latest visible todo and append it to the todos array in the state.

Lets first define a GraphQL query that takes this `latestVisibleId` as an argument and fetches all the tasks that have `id` greater than this `latestVisibleId`. Go to `src/GraphQLQueries.re` and add the following code at the bottom:

```js
module GetNewPublicTodos = [%graphql
  {|
    query getNewPublicTodos ($latestVisibleId: Int!) {
      todos(where: { is_public: { _eq: true}, id: {_gt: $latestVisibleId}}, order_by: { id: desc }) {
        id
        title
        user {
          name
        }
      }
    }
  |}
];
```

Lets now go back to `src/todo/TodoPublicList.re` and write a function that looks at the `latestVisbleId` in the local state and makes the above query to fetch the new todos and finally syncs them with the todos in local state.

Write this function in the `make` function of `src/todo/TodoPublicList.re`:

```js
let fetchNewerTodos = () => {
  let fetchNewerTodosQuery = GraphQLQueries.GetNewPublicTodos.make(~latestVisibleId=latestVisibleId, ());
  let query = {
    "query": ApolloClient.gql(. fetchNewerTodosQuery##query),
    "variables": fetchNewerTodosQuery##variables
  };
  let apolloData = c##query(query);
  apolloData
  |> Js.Promise.then_(gqlResp => {
    let resp = toApolloResult(gqlResp);
    let newTodos = Array.append(resp##data##todos, state##todos);
    dispatch(SetTodos(newTodos));
    Js.Promise.resolve();
  })
  |> ignore
}
```

Now add this function in the `onClick` handler of the `newTodosBanner`.

```
    let newTodosBanner = if(shouldShowNewTodosBanner) {
-      <div className={"loadMoreSection"}>
+      <div className={"loadMoreSection"} onClick={_ => fetchNewerTodos()}>
        {ReasonReact.string("New tasks have arrived!")}
      </div>
    } else {
      {ReasonReact.null}
    };

```

With this, your fully functional todo app is ready.