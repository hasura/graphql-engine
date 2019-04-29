---
title: "Query Variables"
---

What is a variable in GraphQL context?
-------------------------------------
GraphQL has a first-class way to factor dynamic values out of the query, and pass them as a separate dictionary. These values are called variables. In our case, we are defining the object to be inserted as a mutation.

So let's define the graphql mutation to be used with variables.

Open `src/components/TodoInput.js` and add the following code:

```javascript
<script>
+ import gql from "graphql-tag";
+ const ADD_TODO = gql`
+   mutation insert_todos($todo: String!, $isPublic: Boolean!) {
+     insert_todos(objects: {title: $todo, is_public: $isPublic}) {
+       affected_rows
+       returning {
+         id
+         title
+         is_completed
+         created_at
+         is_public
+       }
+     }
+   }
+ `;
  export default {
    props: ['type'],
    data() {
      return {
        newTodo: '',
      }
    },
    methods: {
      addTodo: function () {
        // insert new todo into db
      },
    }
  }
</script>
```

What does this mutation do?
---------------------------
The mutation inserts into `todos` table with the $todo and $isPublic variables being passed.

Awesome! We have defined our first graphql mutation.