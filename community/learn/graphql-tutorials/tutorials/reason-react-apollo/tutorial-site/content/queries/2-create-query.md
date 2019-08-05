---
title: "<Query> component"
metaTitle: "Apollo Query Component | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "We will see how to use Apollo Client Query component in ReasonML. It is a render prop API to fetch data and handle data, loading and error props"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

In this section, we will implement GraphQL Queries and integrate with the UI.
With Apollo Client, you can send queries in 2 different ways.

1. Using the `query` method directly and then process the response.
2. React's render prop API

The more convenient method is to use the render prop method, where you will just pass your GraphQL query as prop and `<Query />` component will fetch the data automatically and will present it in the component's render prop function.

Great! Now let's define the graphql query to be used:

Create a file called `src/GraphQLQueries.re` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/GraphQLQueries.re" text="GraphQLQueries.re" />

```javascript
// GraphQL query module for getting my todos
module GetMyTodos = [%graphql
  {|
    query getMyTodos {
      todos(where: { is_public: { _eq: false} }, order_by: { id: desc }) {
        id
        title
        is_completed
        is_public
      }
    }
  |}
];
module GetMyTodosQuery = ReasonApollo.CreateQuery(GetMyTodos);
```

In the above code,

- `GetMyTodos` is a query module built from plain query string using `graphql_ppx`. If you are coming from JS, it is analogous to wrapping the query in `gql`
- `GetMyTodosQuery` is a typed React component that makes the GraphQL query and provides the request state and the GraphQL data in a render prop. It is similar to the `<Query>` component in react-apollo.


What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

The query is now ready, let's integrate it with our code.

Firstly, let us remove the sample todos.


<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/todo/TodoPrivateList.re" text="TodoPrivateList.re" />

```js
-// sample data
-let sampleTodos = [|
-  {
-    id: 1,
-    title: "This is private todo 1",
-    is_completed: true,
-    is_public: false,
-    user: None,
-  },
-  {
-    id: 2,
-    title: "This is private todo 2",
-    is_completed: false,
-    is_public: false,
-    user: None
-  }
-|];

```

Now let us use the React Component `GetMyTodosQuery` to load the personal todos from the GraphQL server. We will consume the GraphQL response object from the GraphQL response instead of the sample data.


```js
+  <GraphQLQueries.GetMyTodosQuery>
+     ...{
+        ({result}) => switch(result) {
+          | Data(data) => {
              let filteredTodos =
-                sampleTodos
+                data##todos
                |> Array.to_list
                |> List.filter((todo) => switch(todo##is_completed) {
                    | true => state.filter === "all" || state.filter === "complete"
                    | false => state.filter === "all" || state.filter === "active"
                  })
                |> List.map((t) => <TodoItem todo={t} />);

              // filter callback
              let filterTodos = (f) => {
                dispatch(UpdateFilter(f))
              };
              // return JSX
              <React.Fragment>
                <div className="todoListWrapper">
                  <ul>
                    {ReasonReact.array(Array.of_list(filteredTodos))}
                  </ul>
                </div>
                <TodoFilters
                  todoCount={List.length(filteredTodos)}
                  currentFilter={state.filter}
                  filterFunc={filterTodos}
                />
              </React.Fragment>
           }
          | _ => ReasonReact.null
+     }
+  <GraphQLQueries.GetMyTodosQuery>
```

Woot! You have written your first GraphQL integration with ReasonReact. Easy isn't it?

How does this work?
-------------------
When you wrapped your return with `<GetMyTodosQuery>` component, Apollo provides the render prop function with a Union type of `Loading`, `Error(error)` and `Data(data)`. You can pattern match over the argument of the render prop and figure out the status of the GraphQL request. Currently, we are rendering data only if it matches with `Data(data)`, in the next section, we will handle the `Loading` and `Error` states.

`Data(data)`: A type containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

Using the `data` prop, we are parsing the results from the server. In our query, `data` prop has an array `todos` which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed.
