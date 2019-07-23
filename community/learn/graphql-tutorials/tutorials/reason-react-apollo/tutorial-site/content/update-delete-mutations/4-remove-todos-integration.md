---
title: "Remove todos integration"
metaTitle: "GraphQL mutation delete | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "We will use the Apollo Mutation component from ReasonML with variables as an example to delete existing data and update cache using refetchQueries"
---

import GithubLink from "../../src/GithubLink.js";

Lets define the graphql mutation to delete the todo. Open `src/GraphQLQueries.re` and add the following code below everything else:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/GraphQLQueries.re" text="GraphQLQueries.re" />

```javascript
// GraphQL mutation for removing a todo
module DeleteMyTodo = [%graphql
  {|
    mutation removeTodo ($id: Int!) {
      delete_todos(where: {id: {_eq: $id}}) {
        affected_rows
      }
    }
  |}
];
module DeleteMyTodoMutation = ReasonApollo.CreateMutation(DeleteMyTodo);
```

Lets integrate this mutation such that, clicking the cross button next to a todo would remove that todo. Go to `src/todo/TodoItem.re`, and wrap the `x` button with the `DeleteMyTodoMutation` component that we defined above.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/todo/TodoItem.re" text="TodoItem.re" />

```js
+<GraphQLQueries.DeleteMyTodoMutation>
+  ...{
+    (deleteTodo, _) => {
+      let removeTodo = GraphQLQueries.DeleteMyTodo.make(~id=todo##id, ());
-     <button className="closeBtn">
+      <button
+        className="closeBtn"
+        onClick={
+          ev => {
+            deleteTodo(
+              ~variables=removeTodo##variables,
+              ~refetchQueries=[|"getMyTodos"|],
+              ()
+            ) |> ignore;
+          }
+        }
+      >
        {ReasonReact.string("x")}
      </button>
+    }
+  }
+</GraphQLQueries.DeleteMyTodoMutation>
```

In the above code snippet, we are calling the `deleteTodo` mutation in the onClick handler of the button. We call the mutation with `variables` and the `refetchQueries` arguments such that the appropriate todo gets deleted and the database is in sync with the client. Try it out.

Awesome. You have integrated the delete mutation.