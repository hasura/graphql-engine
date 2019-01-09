# Updating and deleting todos

## Toggle todos between complete and active

Any todo app needs to have the functionality of marking the task as complete. In our app, this feature can be implemented using the update mutation.

The mutation is (`updateTodo`):

```graphql
mutation ($id: Int, $isCompleted: Boolean) {
  update_todos (
    _set: {
      is_completed: $isCompleted,
      updated_at: "now()"
    },
    where: {
      id: {
        _eq: $id
      }
    }
  ) {
    returning {
      id
      text
      is_completed
      created_at
      updated_at
      is_public
    }
  }
}
```

The above mutation sets a todo with a particular id (`id` variable) to a particular state (`is_completed` variable).

### UI Implementation

Each of our todos is rendered by the `TodoItem` (`src/screens/components/TodoItem`) component.

Firstly import the necessary components:

```js
import gql from 'graphql-tag';
import { Mutation } from 'graphql-tag';
import CenterSpinner from './CenterSpinner'; // for handling loading state
```

In this component, we can wrap the todo text in a `<Mutation>` component and perform the update mutation whenever the text is pressed. Wrap the `<TouchableOpacity>` button in the `TodoItem` as follows:

```js
<Mutation
  mutation={updateTodo}
  variables={{
    id: item.id,
    isCompleted: !item.is_completed
  }}
>
  {
    (updateTodo, { data, loading, error}) => {
      if (error) {
        return (<Text> Error </Text>);
      }
      const update = () => {
        if (loading) { return; }
        updateTodo();
      }
      return (
        <TouchableOpacity onPress={update}>
          <Text style={item.is_completed ? styles.completedText : styles.activeText}>
            {item.text}
          </Text>
        </TouchableOpacity>
      )
    }
  }
</Mutation>
```

### Updating the UI

In the previous section, for the insert mutation, we gave an `update` prop to the mutation component to update the UI after the mutation. You can provide a similar `update` prop in this case as well but we will leverage Apollo's automatic updates to update the UI.

If the `__typename` of a mutation response or one of the fields of mutation resposne has the `__typename` of a query already present in the cache, Apollo matches the `id` of the two and updates the cache with the data from the `returning` field.

> Note: As the name suggests, this `authomatic updation` works only in case of `update`.

## Deleting the Todos

To delete the todos, we simply have to use a delete mutation and update the cache by passing the `update` prop to the `<Mutation>` component.

The delete mutation is (`deleteTodo`):

```js
mutation ($id: Int) {
  delete_todos (
    where: {
      id: {
        _eq: $id
      }
    }
  ) {
    affected_rows
  }
}
```

To functionalize the delete icon, wrap the delete icon in a `<Mutation>` component similar to how we did with insert and update. However, in this case the `update` prop removes the element from cache.

```js
<Mutation
  mutation={deleteTodo}
  variables={{
    id: item.id,
  }}
  update={(cache) => {
    const data = cache.readQuery({
      query: fetchTodos,
      variables: {
        isPublic,
      }
    });
    const newData = {
      todos: data.todos.filter((t) => t.id !== item.id)
    }
    cache.writeQuery({
      query: fetchTodos,
      variables: {
        isPublic,
      },
      data: newData
    });
  }}
>
  {
    (deleteTodo, { loading, error }) => {
      if (error) {
        return <Text> Error </Text>;
      }
      const remove = () => {
        if (loading) { return; }
        deleteTodo();
      };
      return (
        <View style={styles.deleteButton}>
          <Icon
            name="delete"
            size={26}
            onPress={remove}
            disabled={loading}
            color={loading ? "lightgray" : "#BC0000"}
          />
        </View>
      );
    }
  }
</Mutation> 
```

## Making the user button functional

In case of public todos, each todo has a user button that shows the first letter of the name of the todo maker. Add the following just above the update mutation component.

```js
{ isPublic && <UserItem username={item.user.name}/>}
```

## Wrapping up

If you have done everything correctly, you must have a fully functional basic todo app.

In the upcoming sections, we will demonstrate the use of some advanced Apollo concepts in the app followed by some really interesting features like `Realtime users list` and `Realtime public feed`.
