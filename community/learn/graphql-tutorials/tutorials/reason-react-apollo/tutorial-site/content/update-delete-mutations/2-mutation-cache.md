---
title: "Mutation and update cache"
metaTitle: "GraphQL mutation update and Automatic cache updates | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "We will use the Apollo Mutation component from react-apollo as an example to modify existing data and update cache automatically"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

Now let's do the integration part.

Firstly, let's define the graphql mutation to update the completed status of the todo. Open `src/GraphQLQueriess.re` and add the following code below everything else:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/GraphQLQueries.re" text="GraphQLQueries.re" />

```javascript
// GraphQL mutation for toggling a todo
module ToggleMyTodo = [%graphql
  {|
    mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
      update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
        returning {
          id
          title
          created_at
          is_completed
        }
      }
    }
  |}
];
module ToggleMyTodoMutation = ReasonApollo.CreateMutation(ToggleMyTodo);
```


In the previous section, we performed `insert` mutation. We will follow the same steps for toggling todos between complete and active by using the `update` mutation.

Go to `src/todo/TodoItem.re`, and wrap the checkbox with the `ToggleMyTodoMutation` component that we defined above.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/todo/TodoItem.re" text="TodoItem.re" />

```js
+<GraphQLQueries.ToggleMyTodoMutation>
+  ...{
+    (mutate, _) => {
+      let toggleTodo = GraphQLQueries.ToggleMyTodo.make(~id=todo##id, ~isCompleted=!todo##is_completed, ());
      <input
        type_="checkbox"
        checked={todo##is_completed}
        id={string_of_int(todo##id)}
+        onChange={
+          event => {
+            mutate(
+              ~variables=toggleTodo##variables,
+              ()
+            ) |> ignore;
+          }
+        }
      />
+    }
+  }
+</GraphQLQueries.ToggleMyTodoMutation>
```

The above code will just make a mutation, updating the todo's `is_completed` property in the database. If you see in the above code snippet, we are not doing anything to updating the cache, but if you try it out in the UI, the mutation succeeds and the UI is also updated.

This happens because, in case of update mutation, apollo client tries to keep the cache fresh by performing automatic updates using the following strategy:

1. It looks at the `id` and `__typename` of the mutation response
2. It looks for the objects in the cache that have `id` and `__typename` similar to the ones in the mutation response.
3. If there is a match, it updates the cache with the data from the mutation response

