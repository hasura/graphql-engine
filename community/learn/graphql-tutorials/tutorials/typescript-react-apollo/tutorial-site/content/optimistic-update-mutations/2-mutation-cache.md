---
title: "Mutation and update cache"
metaTitle: "Apollo useMutation hook for optimistic update | GraphQL React Apollo Typescript Tutorial"
metaDescription: "We will use the Apollo useMutation hook from @apollo/react-hooks as an example to modify existing data and update cache locally using readQuery and writeQuery and handle optimisticResponse"
---

import GithubLink from "../../src/GithubLink.js";

Now let's do the integration part. Open `src/components/Todo/TodoItem.tsx` and add the following code below the other imports:

```javascript
  import * as React from 'react';
+ import gql from 'graphql-tag';
  import { Todos } from '../../generated/graphql';
```
Let's define the graphql mutation to update the completed status of the todo

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/Todo/TodoItem.tsx" text="src/components/Todo/TodoItem.tsx" />

```javascript
  
  interface TodoItemType {
    index: number,
    todo: Partial<Todos>
  };

+ const TOGGLE_TODO = gql`
+   mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
+     update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
+       affected_rows
+     }
+   }
+ `;

```

### Apollo useMutation Hook
We need to use the `useMutation` hook to make the mutation. So let's import it.

```javascript

  import * as React from 'react';
  import gql from 'graphql-tag';
+ import { useMutation } from '@apollo/react-hooks';
  import { Todos } from '../../generated/graphql';

```

And now, let's use the hook inside the TodoItem component.

```javascript

  const TodoItem = ({index, todo}: TodoItemType) => {

+   const [todoUpdate] = useMutation(TOGGLE_TODO);

```

We already have the onChange handler toggleTodo for the input. Let's update the function to trigger the mutation. As we saw earlier, the `mutate` function takes optional arguments like variables and update. In addition, it also accepts `optimisticResponse` to update the UI before the actual result arrives.

```javascript
  const toggleTodo = () => {
+   todoUpdate({
+     variables: { id: todo.id, isCompleted: !todo.is_completed },
+     optimisticResponse: {
+       __typename: "Mutation",
+       update_todos: {
+         __typename: "todos_mutation_response",
+         id: todo.id,
+         title: todo.title,
+         is_completed: todo.is_completed,
+         affected_rows: 1
+       }
+     }
+   });
  };
```

The above code will just make a mutation, updating the todo's is_completed property in the database.

To update the cache, we will be using the `update` function again to modify the cache. We need to fetch the current list of todos from the cache before modifying it. So let's import the query.

```javascript

  import * as React from 'react';
  import gql from 'graphql-tag';
  import { useMutation } from '@apollo/react-hooks';
+ import { GET_MY_TODOS } from './TodoPrivateList';

```
Now let's add the code for `update` function.

```javascript

  const TodoItem = ({index, todo}: TodoItemType) => {

-   const [todoUpdate] = useMutation(TOGGLE_TODO);
+   const [todoUpdate] = useMutation(
+     TOGGLE_TODO, 
+     {
+       update(cache, { data }) {
+         const existingTodos : any = cache.readQuery({ query: GET_MY_TODOS });
+         const newTodos = existingTodos!.todos.map((t:any) => {
+           if (t.id === todo.id) {
+             return({...t, is_completed: !t.is_completed});
+           } else {
+             return t;
+           }
+         });
+         cache.writeQuery({
+           query: GET_MY_TODOS,
+           data: {todos: newTodos}
+         });
+       }
+     }
+   );

```

We are fetching the existing todos from the cache using `cache.readQuery` and updating the is_completed value for the todo that has been updated.

Finally we are writing the updated todo list to the cache using `cache.writeQuery`.

### Mapping Types

Now that the update mutation is completed, let's add type safety. We need type definitions for the the cache update portions.

```javascript

  import { GET_MY_TODOS } from './TodoPrivateList';
- import { Todos } from '../../generated/graphql';
+ import { GetMyTodosQuery, Todos } from '../../generated/graphql';

```

Now let's add it to both readQuery and writeQuery.

```javascript

  const [todoUpdate] = useMutation(
     TOGGLE_TODO, 
     {
       update(cache, { data }) {
         const existingTodos = cache.readQuery({ query: GET_MY_TODOS });
+        const existingTodos = cache.readQuery<GetMyTodosQuery>({ query: GET_MY_TODOS });
         const newTodos = existingTodos!.todos.map(t => {
           if (t.id === todo.id) {
             return({...t, is_completed: !t.is_completed});
           } else {
             return t;
           }
         });
-        cache.writeQuery({
+        cache.writeQuery<GetMyTodosQuery>({
           query: GET_MY_TODOS,
           data: {todos: newTodos}
         });
       }
     }
   );
```




