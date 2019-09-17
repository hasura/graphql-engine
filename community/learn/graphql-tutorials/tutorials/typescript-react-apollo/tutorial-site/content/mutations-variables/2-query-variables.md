---
title: "Query Variables"
metaTitle: "Passing GraphQL Variables in Queries | GraphQL React Apollo Typescript Tutorial"
metaDescription: "An Example of passing variables in GraphQL context and usage of Apollo GraphQL Mutation variables in React app."
---

import GithubLink from "../../src/GithubLink.js";

What is a variable in GraphQL context?
-------------------------------------
GraphQL has a first-class way to factor dynamic values out of the query, and pass them as a separate dictionary. These values are called variables. In our case, we are defining the object to be inserted as a mutation.

So let's define the graphql mutation to be used.

Open `src/components/Todo/TodoInput.tsx` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/Todo/TodoInput.tsx" text="src/components/Todo/TodoInput.tsx" />

```javascript

  import * as React from 'react';
+ import gql from 'graphql-tag';
+ const ADD_TODO = gql `
+   mutation insert_todos($todo: String!, $isPublic: Boolean!) {
+     insert_todos(objects: {title: $todo, is_public: $isPublic}) {
+       affected_rows
+       returning {
+         id
+         title
+         is_completed
+       }
+     }
+   }
+ `;

  const TodoInput = ({isPublic=false}) => {
    const [todoInput, setTodoInput] = React.useState('');
    return (
      <form className="formInput" onSubmit={(e) => {
        e.preventDefault();
        // add todo
      }}>
        <input
          className="input"
          placeholder="What needs to be done?"
          value={todoInput}
          onChange={e => (setTodoInput(e.target.value))}
        />
        <i className="inputMarker fa fa-angle-right" />
      </form>
    );
  };

  export default TodoInput;

```

What does this mutation do?
---------------------------
The mutation inserts into `todos` table with the $todo and $isPublic variables being passed.

Awesome! We have defined our first graphql mutation.
