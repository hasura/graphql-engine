# Apollo cache custom manipulation

We can use Apollo cache for maintaining custom states just like `this.state` or `redux`. Let us demonstrate this while implementing the `load older todos` feature

## Load more todos

Currently, our app only fetches the latest 10 todos at first. Let us implement the `load more` feature so that we can load older todos as well.

### Setup

For this feature, we will store the following fields in the Apollo cache:

- `loadMoreTextPrivate`: (String) which is either `Load older todos` or `No more todos` on the Private todos screen.
- `loadMoreTextPublic`: (String) which is either `Load older todos` or `No more todos` on the Public todos screen.
- `loadMoreButtonEnabledPrivate`: (Boolean) which stores whether the `Load older todos` button on the Private todos screen should be pressable. (The button must not be pressable when one request is already in progress)
- `loadMoreButtonEnabledPublic`: (Boolean) which stores whether the `Load older todos` button on the Public todos screen should be pressable. (The button must not be pressable when one request is already in progress)

Let us first write a utility function called `updateCache` function that updates a particular field in the cache. If you are thinking in redux, this function is similar to the reducer action that updates the state. It reads the current state and updates the state with new key.

```js
updateCache = (key, value) => {
  const { client } = this.props;
  const resp = client.query({query: gql`{
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
```

Let us set the initial value of `loadMoreTextPrivate` and `loadMoreTextPublic` to 'Load more todos' and `loadMoreButtonEnabledPrivate` and `loadMoreButtonEnabledPublic` to true. Since this is a synchronous action, we can set these initial values in the constructor.

```js
this.props.client.writeData({
  data: {
    loadMoreButtonEnabledPrivate: true,
    loadMoreButtonEnabledPublic: true,  
    loadMoreTextPrivate: 'Load more todos',
    loadMoreTextPublic: 'Load more todos',
  }
});
```

### Query

The query to load more todos looks like (`fetchOldTodos`):

```js
const fetchOldTodos = gql`
  query ($lastId: Int, $isPublic: Boolean){
    todos (
      order_by: id_desc,
      where: {
        _and: {
          is_public: { _eq: $isPublic},
          id: { _lt: $lastId}
        }
      },
      limit: 10
    ) {
      id
      text
      is_completed
      created_at
      updated_at
      is_public
      user {
        name
      }
    }
  }
`;
```

This query fetches 10 todos older than the oldest local todo in our cache.

### Fetching todos and appending to cache

The `Load more todos` button should be inside the scrollview so that it appears only when the user is scrolled to the bottom. On pressing the button, we must:

In the sections below, `loadMoreButtonEnabled` refers to `loadMoreButtonEnabledPrivate` in case of private todos and `loadMoreButtonEnabledPublic` otherwise. Similar for `loadMoreText`. 

1. Set `loadMoreButtonEnabled` to false so that the user cannot click it repeatedly and unnecessary requests don't happen.
2. Fetch 10 older todos (using the above query)
3. Add the older todos to Apollo cache
4. If the number of old todos are less than 10, then set `loadMoreText` to `No more todos` so that the user does not click it again.
5. If the numbero of todos are equal to 10, then set `paginateButtonEnabled` back to true.

Let us write a function `fetchOldTodos` that does the two above steps.

```js
fetchOldTodos = async () => {
  const { client, isPublic } = this.props;
  const todoType = isPublic ? 'Public' : 'Private';
  const data = client.readQuery({
    query: fetchTodos,
    variables: {
      isPublic,
    }
  });
  const response = await client.query({
    query: fetchOldTodos,
    variables: {
      isPublic,
      lastId: data.todos[data.todos.length - 1].id
    },
  });
  if (!response.data) {
    return;
  }
  if (response.data.todos) {
    client.writeQuery({
      query: fetchTodos,
      variables: {
        isPublic
      },
      data: { todos: [ ...data.todos, ...response.data.todos]}
    });
    if (response.data.todos < 10) {
      this.updateCache(`loadMoreText${todoType}`, 'No more todos');  
    } else {
      this.updateCache(`loadMoreButtonEnabled${todoType}`, true);
    }
  } else {
    this.updateCache(`loadMoreText${todoType}`, 'No more todos');  
  }
}
```

### UI elements

We will write a new component.

- LoadMoreButton: A button that subscribes to the following fields:
  - `loadMoreButtonEnabledPrivate`
  - `loadMoreButtonEnabledPublic`
  - `loadMoreTextPrivate`
  - `loadMoreTextPublic`

The interesting part is that we can use the same `<Query>` components while making queries to the local cache as well. The only differences are:

1. We do not have to worry about the loading and error state because querying the local cache is a synchronous action.
2. You have to add a directive called `@client` next to the field.

The query would be:

```graphql
query {
  loadMoreButtonEnabledPrivate @client
  loadMoreButtonEnabledPublic @client
  loadMoreTextPrivate @client
  loadMoreTextPublic @client
}
```

Let us make the `LoadMoreButton` and pass the `fetchOlderTodos` function, `updateCache` function and the `todoType` ('Public' or 'Private') to it as props.


**LoadMoreButton**
```js
const LoadMoreButton = ({updateCache, fetchOlderTodos, todoType}) => (
  <Query
    query={gql`{
      loadMoreButtonEnabledPrivate @client
      loadMoreButtonEnabledPublic @client
      loadMoreTextPrivate @client
      loadMoreTextPublic @client
    }`}
  >
    {
      ({data}) => {
        return (
          <TouchableOpacity
            style={styles.pagination}
            onPress={() => {
              updateCache(`loadMoreButtonEnabled${todoType}`, false);
              fetchOlderTodos();
            }}
            disabled={!data[`loadMoreButtonEnabled${todoType}`]}
          > 
            {
              !data[`loadMoreButtonEnabled${todoType}`] && data[`loadMoreText${todoType}`] !== 'No more todos' ?
              <CenterSpinner /> :
              <Text style={styles.buttonText}> {data[`loadMoreText${todoType}`]} </Text>
            }
          </TouchableOpacity> 
        )
      }
    }
  </Query>
);
```

**Todos**
```js
render () {
  return (
    ...
    <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
      <FlatList
        data={data.todos}
        renderItem={({item}) => <TodoItem item={item} isPublic={this.props.isPublic}/>}
        keyExtractor={(item) => item.id.toString()}
      />
      <LoadMoreButton updateCache={this.updateCache} fetchOlderTodos={this.fetchOlderTodos} todoType={todoType}/>
    </ScrollView>
    ...
  )  
}
```

We make the button's `onPress` function to set the `paginateButtonEnabled` to false and then fetch the older todos.

## Wrapping up

If you did everything correctly, you might have a functional button at the bottom of the 10 todos that fetches the previous 10 todos when pressed.

In the next section, we will make a realtime online users list.
