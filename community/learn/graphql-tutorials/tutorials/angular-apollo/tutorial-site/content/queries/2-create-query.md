---
title: "watchQuery method"
metaTitle: "Apollo Query Component | GraphQL Angular Apollo Tutorial"
metaDescription: "We will use the Apollo Client watchQuery from apollo-angular. It is a API to fetch data and handle data, loading and error fields"
---

import GithubLink from "../../src/GithubLink.js";

In this section, we will implement GraphQL Queries and integrate with the angular UI.
With Apollo Client, you can send queries in 2 different ways.

1. Using the query method.
2. Using the watchQuery method. (Recommended)

### Apollo watchQuery Method 
query method returns an Observable that emits a result, just once. watchQuery also does the same, except it can emit multiple results. (The GraphQL query itself is still only sent once, but the watchQuery observable can also update if, for example, another query causes the object to be updated within Apollo Client's global cache.)

Great! Now let's define the graphql query to be used:

Open `src/app/Todo/TodoPrivateList.ts` and add the following code:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/angular-apollo/app-final/src/app/Todo/TodoPrivateList.ts" text="src/app/Todo/TodoPrivateList.ts" />

```typescript
import { Component, OnInit, Input } from '@angular/core';

import {TodoItem} from "./TodoItem";
import {TodoFilters} from "./TodoFilters";
+ import gql from 'graphql-tag';

+ const GET_MY_TODOS = gql`
+  query getMyTodos {
+    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
+      id
+      title
+      created_at
+      is_completed
+  }
+ }`;
```

We have now written the graphql query as a typescript constant using the `gql` parser function. This function is used to parse the plain string as a graphql query.

What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

The query is now ready, let's integrate it with our angular code.

```typescript

+ import { Apollo } from 'apollo-angular';
```

`Apollo` is being imported from `apollo-angular`

```typescript

import { Component, OnInit  } from '@angular/core';

import {TodoItem} from "./TodoItem";
import {TodoFilters} from "./TodoFilters";
import { Apollo } from 'apollo-angular';
import gql from 'graphql-tag';

export const GET_MY_TODOS = gql`
  query getMyTodos {
    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
      id
      title
      created_at
      is_completed
  }
 }`;

@Component({  
    selector: 'TodoPrivateList',  
    templateUrl: './TodoPrivateList.template.html'
  }) 

export class TodoPrivateList implements OnInit {
    
filter = "all";
clearInProgress= false;
todos: [
        {
          id: "1",
          title: "This is private todo 1",
          is_completed: true,
          is_public: false
        },
        {
          id: "2",
          title: "This is private todo 2",
          is_completed: false,
          is_public: false
        }
       ];
filteredTodos: any;
+ loading: boolean = true;

+ constructor(private apollo: Apollo) {}

+ ngOnInit() {
+   this.apollo.watchQuery<any>({
+    query: GET_MY_TODOS
+  })
+   .valueChanges
+   .subscribe(({ data, loading }) => {
+      this.loading = loading;
+      this.todos = data.todos;
+      this.filteredTodos = this.todos; 
+  });
+ }

filterResults($event) { 
  ... 
}
 
clearCompleted() {}

}

```

We are importing `Apollo` from `apollo-angular` and the graphql query we defined above to fetch the todo data.

Let's remove the mock `todos` data which was used to populate sample data.

```typescript

export class TodoPrivateList implements OnInit {
    
filter = "all";
clearInProgress= false;
todos= [
-        {
-          id: "1",
-          title: "This is private todo 1",
-          is_completed: true,
-          is_public: false
-        },
-        {
-          id: "2",
-          title: "This is private todo 2",
-          is_completed: false,
-          is_public: false
-        }
        ]
filteredTodos: any;
loading: boolean = true;

constructor(private apollo: Apollo) {}

ngOnInit() {
  ...
}

filterResults($event) { 
  ... 
}
 
clearCompleted() {}

}

```

Woot! You have written your first GraphQL integration with Angular. Easy isn't it?

How does this work?
-------------------
The watchQuery method, returns an Observable, which has a subscribe method with the data and few other fields.  Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

Using the `data` field, we are parsing the results from the server. In our query, `data` field has an array `todos` which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed.
