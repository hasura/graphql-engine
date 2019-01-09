# Refetch todos for public todos

As of now, we are loading all the public todos at once when the component loads. But we do not have the todos that other people might have added after we loaded the app. Let us create a button to reload new todos.

We will use the following query:

```graphql
  query ($lastId: Int){
    todos (
      order_by: id_desc,
      where: {
        and: {
          is_public: { _eq: true},
          id: { _gt: $lastId}
        }
      }
    ) {
      id
      text
      is_completed
      user {
        name
      }
      created_at
    }
  }
```

As you see, this query fetches all the public todos that have `id` greater than the `lastId` which is taken as a query variable. Since the `id` in the `todo` table is set to auto-increment, this query makes sure that it fetches only the todos newer than the ones we have in the cache.

To implement this refetch button, we will need to do the following:

1. Read the cache to see the current todos and find the `id` of the latest todo.

    ```js
    const data = client.readQuery({
      query: fetchTodos,
      variables: {
        isPublic,
      }
    });
    const lastId = data.todos[0].id;
    ```

2. Manually fire a query using the `client.query` function to fetch the new todos.

    ```js
    const resp = await client.query({
      query: fetchNewTodos,
      variables: { lastId }
    }); 
    ```

3. Append these new todos to the older todos and write it to the cache so that the UI updates. 

    ```js
    if (resp.data) {
      const newData = {
        todos: [ ...resp.data.todos, ...data.todos]
      }
      client.writeQuery({
        query: fetchTodos,
        variables: {
          isPublic,
        },
        data: newData
      });
    }
    ```

4. Let us include the above logic in a new function. Create a `fetchNewTodos` function inside our `Todos` component (`src/screens/components/Todos.js`). The function looks like this:

    ```js
    fetchNewTodos = async () => {
      const { client, isPublic } = this.props;
      const data = client.readQuery({
        query: fetchTodos,
        variables: {
          isPublic,
        }
      });
      const lastId = data.todos[0].id;
      const resp = await client.query({
        query: fetchNewTodos,
        variables: { lastId }
      });
      if (resp.data) {
        const newData = {
          todos: [ ...resp.data.todos, ...data.todos]
        }
        client.writeQuery({
          query: fetchTodos,
          variables: {
            isPublic,
          },
          data: newData
        });
      }
    }
    ```

4. Finally add a button that calls the above function `onPress`. Add it just next above the `ScrollView` in our `Todos` component.

    ```js
    <TouchableOpacity
      onPress={this.fetchNewTodos}
      style={styles.banner}
    >
      <Text> Load new todos </Text>
    </TouchableOpacity> 
    ```

## Wrapping up

If you did everything correctly, you must have a button at the top of the todos which on pressed fetches the new public todos if any are available.

Now, lets make this feed update realtime. In the next section we will:
1. Capture realtime updates about new public todos have been added by others
2. Show the load more button only if new todos exist