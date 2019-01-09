# Capturing Realtime Updates

In the previous section we implemented a button that refetches the todos newer than than the ones we have locally.

Now let us convert this public todos feed into a realtime feed (like twitter) by showing this button only when new todos are available. This is when the realtime subscriptions will come in handy. The pattern we follow for this realtime feed is:

1. Subscribe to the last public todo added in `componentDidMount`.
2. Whenever an event occurs, show a button (in the form of a notification) that fetches the new todos and adds it to the local cache.

You might be wondering why do not we use the Apollo `<Subscription>` component. Imagine a scenario when we have 100 public todos in the database and we have all of them in our local cache. Now whenever there is a new todo added, the `<Subscription>` component receives the 100 todos along with the new todos. As you see, we are unnecessarily receiving the data that we already have. Therefore, we just use subscriptions as event notifications and run custom logic on event data.

Let us implement it.

1. The subscription that we will use is (`subscribeToNewTodos`) :

    ```graphql
    subscription {
      todos (
        order_by: id_desc
        limit: 1
        where: { is_public: { _eq: true }}
      ) {
        id
      }
    }
    ```

    This subscription will give us an event whenever a new todo gets added. Let start this subscription in `componentDidMount` only if we are viewing public todos.

    ```js
    async componentDidMount() {
      const { client, isPublic } = this.props;
      if (isPublic) {
        client.subscribe({
          query: subscribeToNewTodos,
        }).subscribe({
          next: (event) => {
            ...
            //handle event
          },
          error: (err) => {
            console.error("err", err);
          }
        })
      }
    }
    ```

2. On receiving an event, if the todo in the event has `id` greater than the `id` of latest todo that we have locally, we update a key called `newTodosExist` in the Apollo cache and set it to `true`.

    ```js
    if (event.data.todos.length) {
      let localData;
      try {
        localData = client.readQuery({
          query: fetchTodos,
          variables: {
            isPublic: true,
          }
        });
      } catch (e) {
        return;
      } 
      const lastId = localData.todos[0] ? localData.todos[0].id : 0
      if (event.data.todos[0].id > lastId) {
        client.writeData({data: {newTodosExist: true}});
      }
    }
    ```


    Also add this `newTodosExist` key in the `constructor` and the `updateCache` function. These functions should finally look like:

    ```js
    export default class Todos extends React.Component {
         
      constructor (props) {
        super(props);
        this.props.client.writeData({
          data: {
            newTodosExist: false,
            loadMoreButtonEnabledPrivate: true,
            loadMoreButtonEnabledPublic: true,  
            loadMoreTextPrivate: 'Load more todos',
            loadMoreTextPublic: 'Load more todos',
          }
        });
      }

      updateCache = (key, value) => {
        const { client } = this.props;
        const resp = client.query({query: gql`{
          newTodosExist @client
          loadMoreButtonEnabledPrivate @client
          loadMoreButtonEnabledPublic @client
          loadMoreTextPrivate @client
          loadMoreTextPublic @client
        }`});
        const newData = {
          ...resp.data
        };
        newData[key] = value;
        client.writeData({
          data: {
            ...newData
          } 
        });
      }
      ...
      ...
    }

    ```
   

3. Now we make our `Fetch New todos` button visible only when `newTodosExist` is true and we set `newTodosExist` back to false after refetching the todos.

   Let us move our `Fetch New todos` into a new React component that is dependent on the `newTodosExist` value in the cache.

   ```js
   const NewTodosBanner = (props) => (
      <Query
        query={gql`{ newTodosExist @client }`}
      >
        {
          ({data}) => {
            if (data && data.newTodosExist) {
              return (
                <TouchableOpacity
                  onPress={props.fetch}
                  style={styles.banner}
                >
                  <Text> Load new todos </Text>
                </TouchableOpacity>
              )
            }
            return null;
          }
        }
      </Query>
   )
   ```

   > As you can see above, we have used a `Query` component which shows that we can also use the Query components to query the custom data in the cache. You just have to add a directive called `@client` to query the local data.

   Now, use this `NewTodosBanner` in the render of `Todos` component and pass the `fetchNewTodos` function as a prop.

   ```js
   render() {
      return (
        ...
        { isPublic && <NewTodosBanner fetch={this.fetchNewTodos}/>}
        ...
      );
   }
   ```

   Finally, in the end of the `fetchNewTodos` function, set `newTodosExist` to false.

   ```js
   this.updateCache('newTodosExist', false)
   ```

4. In the `Textbox` component (`src/screens/components/Textbox.js`), in the `update` prop of the `<Mutation>` component, add the following line at the top of the function. We do this so that the todo creations captured by subscriptions are consistent.

   ```js
   if (isPublic) { return ; }
   ```

## Wrapping up

If you have done everything correctly, you must have a fully functional todo app ready.

Check out other tutorials [here](TODO).