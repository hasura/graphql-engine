---
title: "Mapping Types"
metaTitle: " Mapping GraphQL Queries to Typescript | GraphQL React Apollo Typescript Tutorial"
metaDescription: "We will see how to import the auto-generated Typescript types for GraphQL queries to add type safety to Todo component"
---

Let's type safe the `useQuery` so that we parse the right result object state, which includes `loading`, `error` and `data`.

The auto-generated type definitions are available at `src/generated/graphql.tsx` file. Let's import `GetMyTodosQuery` relevant for this component.

```javascript
  import React, { Fragment, useState } from "react";
  import gql from "graphql-tag";
  import { useQuery } from "@apollo/react-hooks";
  import TodoItem from "./TodoItem";
  import TodoFilters from "./TodoFilters";

+ import { 
+   GetMyTodosQuery,
+ } from '../../generated/graphql';

```

Now let's add this to `useQuery`:

```javascript

  const TodoPrivateList = () => {

    const [filter, setFilter] = useState<string>("all");
-   const { loading, error, data } = useQuery(GET_MY_TODOS);
+   const { loading, error, data } = useQuery<GetMyTodosQuery>(GET_MY_TODOS);

    const filterResults = (filter: string): void => {
      setFilter(filter);
    };

```

We also have `Todo` type manually defined in the app boilerplate before the GraphQL integration. Let's import `Todos` type definition from the generated file.

```javascript

  import { 
-   GetMyTodosQuery
+   GetMyTodosQuery,
+   Todos
  } from '../../generated/graphql';

```

Let's update the parts of code where the `Todo` type is being used.

```javascript
  let filteredTodos = data.todos;
  if (filter === "active") {
-   filteredTodos = data.todos.filter((todo: Todo) => todo.is_completed !== true);
+   filteredTodos = data.todos.filter((todo: Partial<Todos>) => todo.is_completed !== true);
  } else if (filter === "completed") {
-   filteredTodos = data.todos.filter((todo: Todo) => todo.is_completed === true);
+   filteredTodos = data.todos.filter((todo: Partial<Todos>) => todo.is_completed === true);
  }

- const todoList = filteredTodos.map((todo: Todo, index: number) => (
+ const todoList = filteredTodos.map((todo: Partial<Todos>, index: number) => (
    <TodoItem
      key={'item'+index}
      index={index}
      todo={todo}
    />
  ));
```

We are using `Partial<Todos>`, instead of `<Todos>` to handle properties that are not used in the UI.

And now with updated todos, let's remove the manually declared type definition for todo.

```javascript

- type Todo = {
-   id: number,
-   title: string,
-   is_completed: boolean
- };

  const TodoPrivateList = () => {

    const [filter, setFilter] = useState<string>("all");
    const { loading, error, data } = useQuery<GetMyTodosQuery>(GET_MY_TODOS);

```

Since we have updated the todo type here, the same needs to be reflected in `TodoItem` component as well.

Open `src/components/Todo/TodoItem.tsx`

```javascript

  import * as React from 'react';
+ import { Todos } from '../../generated/graphql';

- export type TodoItem = {
-   id: number,
-   title: string,
-   is_completed: boolean
- };

  interface TodoItemType {
    index: number,
-   todo: TodoItem
+   todo: Partial<Todos>
  };

```

Again let's update the `TodoFilters` component as well which imports the `TodoItem` type definition.

```javascript

  import * as React from 'react';
- import { TodoItem } from './TodoItem';
+ import { GetMyTodosQuery } from '../../generated/graphql';

  interface filterResults {
    (filter: string): void
  }

  interface TodoFiltersArgs {
-   todos: TodoItem[],
+   todos: GetMyTodosQuery["todos"],
    currentFilter: string,
    filterResultsFn: filterResults,
    clearCompletedFn: VoidFunction
  }

```

Note that here we are importing `GetMyTodosQuery` and getting the type of `todos` property using `GetMyTodosQuery["todos"]`. Here `todos` is an array where as previously we were defining type for a single todo.





