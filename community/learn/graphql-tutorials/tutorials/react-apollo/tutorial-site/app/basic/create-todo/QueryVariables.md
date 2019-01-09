So let's define the graphql mutation to be used.

Open `src/components/Todo/TodoQueries.js` and add the following code:

```
const MUTATION_TODO_ADD = gql`
  mutation insert_todos($objects: [todos_insert_input!]) {
    insert_todos(objects: $objects) {
      affected_rows
      returning {
        id
        text
        is_completed
        created_at
        is_public
      }
    }
  }
`;
export { 
  MUTATION_TODO_ADD
};
```

What does this mutation do?
---------------------------
The mutation inserts into `todos` table with the $objects variable being passed as one todo type.

What is a variable?
-------------------
GraphQL has a first-class way to factor dynamic values out of the query, and pass them as a separate dictionary. These values are called variables. In our case, we are defining the object to be inserted as a mutation.

Awesome! We have defined our first graphql mutation. Now let's do the integration part.