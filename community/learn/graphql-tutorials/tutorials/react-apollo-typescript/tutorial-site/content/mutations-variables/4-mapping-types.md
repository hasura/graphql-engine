---
title: "Mapping Types"
metaTitle: " Mapping GraphQL Mutations to Typescript | GraphQL React Apollo Typescript Tutorial"
metaDescription: "We will see how to import the auto-generated Typescript types for GraphQL mutations to add type safety to TodoInput component"
---

Let's type safe the `useMutation`.

As we learnt in the previous section, the auto-generated type definitions are available at `src/generated/graphql.tsx` file. Let's import `GetMyTodosQuery, Insert_TodosMutation, Insert_TodosMutationVariables` relevant for this component.

```javascript
  import * as React from 'react';
  import gql from 'graphql-tag';
  import { useMutation } from "@apollo/react-hooks";
  import { GET_MY_TODOS } from './TodoPrivateList';
+ import { GetMyTodosQuery, Insert_TodosMutation, Insert_TodosMutationVariables } from '../../generated/graphql';

```

Now let's add this to `useMutation`:

```javascript

  const TodoInput = ({isPublic=false}) => {
    const [todoInput, setTodoInput] = React.useState('');
-   const [addTodo] = useMutation(ADD_TODO);
+   const [addTodo] = useMutation<Insert_TodosMutation, Insert_TodosMutationVariables>(ADD_TODO);
    return (

```

Let's update the cache section which uses `readQuery` and `writeQuery`. We have imported `GetMyTodosQuery` type defintion for this.

```javascript

  <form className="formInput" onSubmit={(e) => {
    e.preventDefault();
    // add todo
    addTodo({
      variables: {todo: todoInput, isPublic },
      update(cache, { data }) {
          // do not update cache for public feed
          if (isPublic || !data) {
            return null;
          }
-         const getExistingTodos : any = cache.readQuery({ query: GET_MY_TODOS });
+         const getExistingTodos = cache.readQuery<GetMyTodosQuery>({ query: GET_MY_TODOS });
          // Add the new todo to the cache
          const existingTodos = getExistingTodos ? getExistingTodos.todos : [];
          const newTodo = data.insert_todos!.returning[0];
-         cache.writeQuery({
+         cache.writeQuery<GetMyTodosQuery>({
            query: GET_MY_TODOS,
            data: {todos: [newTodo, ...existingTodos]}
          });
        }
    });
    setTodoInput('');
  }}>
```

We have now removed the explicit usage of `any` for `getExistingTodos`, replacing it with the auto-generated type.

