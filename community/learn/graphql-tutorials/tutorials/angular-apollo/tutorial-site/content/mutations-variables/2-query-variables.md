---
title: "Query Variables"
metaTitle: "Passing GraphQL Variables in Queries | GraphQL Angular Apollo Tutorial"
metaDescription: "An Example of passing variables in GraphQL context and usage of Apollo GraphQL Mutation variables in Angular app."
---

import GithubLink from "../../src/GithubLink.js";

What is a variable in GraphQL context?
-------------------------------------
GraphQL has a first-class way to factor dynamic values out of the query, and pass them as a separate dictionary. These values are called variables. In our case, we are defining the object to be inserted as a mutation.

So let's define the graphql mutation to be used.

Open `src/app/Todo/TodoInput.ts` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/angular-apollo/app-final/src/app/Todo/TodoInput.ts" text="src/app/Todo/TodoInput.ts" />

```typescript
import { Component, OnInit, Input } from '@angular/core';
+ import gql from 'graphql-tag';

+ const ADD_TODO = gql `
+  mutation ($todo: String!, $isPublic: Boolean!) {
+    insert_todos(objects: {title: $todo, is_public: $isPublic}) {
+      affected_rows
+      returning {
+        id
+        title
+        created_at
+        is_completed
+      }
+    }
+  }
+ `;

@Component({  
    selector: 'TodoInput',  
    templateUrl: './TodoInput.template.html',  
  }) 

export class TodoInput {
  ...
}
```

What does this mutation do?
---------------------------
The mutation inserts into `todos` table with the $objects variable being passed as one todo type.

Awesome! We have defined our first graphql mutation.
