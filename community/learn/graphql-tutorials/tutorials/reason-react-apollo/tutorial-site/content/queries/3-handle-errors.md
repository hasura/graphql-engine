---
title: "Handle loading/errors"
metaTitle: "Apollo Query Component Error Handling | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "We will handle the GraphQL loading and error states in the app using the Apollo Query Component render props - loading and error "
---

As we saw in the previous step, Apollo the GraphQL state into the component’s render prop function. Among them `Loading` and `Error` are common ones that you will need to handle in your app.

`Loading`: When the result matches `Loading`, this means that the GraphQL request is in loading state. This information can be used to display a loading spinner.

`Error(error)`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

Now let's go back to the `GraphQLQueries.GetMyTodosQuery>` component that you wrote in the previous step.

```javascript

  <GraphQLQueries.GetMyTodosQuery>
    ...{
      ({result}) => switch(result) {
+        | Loading => <div>{ReasonReact.string("Loading")}</div>
+        | Error(error) => {
+          Js.Console.error(error);
+          <div>{ReasonReact.string("Loading")}</div>
+        }
        | Data(data) => {
          let filteredTodos =
            data##todos
            |> Array.to_list
            |> List.filter((todo) =>
              switch(todo##is_completed) {
                | true => state.filter === "all" || state.filter === "complete"
                | false => state.filter === "all" || state.filter === "active"
              })
            |> List.map((t) => <TodoItem todo={t} />)

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
-        | _ => ReasonReact.null
      }
    }
  </GraphQLQueries.GetMyTodosQuery>

```

When this component mounts, the GraphQL query sent in the background may not have been completed. But we need to handle that temporary state of no data and hence we return some useful text during `Loading` state. 
In this loading state, typically you can do fancy things like displaying a loading spinner.

Now, the query could also end up in an `Error` state due to various reasons. Sometimes the graphql query could be wrong, or the server isn't responding. Whatever may be the reason, the user facing UI should show something to convey that an error has occurred. 
In this error state, typically you can send these error messages to third-party services to track what went wrong.

All said and done, these are two important states that need to be handled inside your component. What you have written above is basic, but sufficient for this tutorial.
