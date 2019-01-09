# Apollo Cache

Apollo Client comes with a cache that it uses for caching queries. The cache acts like React state for the `Query`, `Mutation` and `Subscription` components. This significantly reduces state management on the developers part by abstracting the repetitive and tedious part.

## Comparison

Let us compare how you would make a GraphQL query without Apollo Client.

### Without Apollo

```js
const query = `query { rootField { field1 field2 }}`

class Comp extends React.Component {
  state = {
    loading: true,
    data: null,
    error: null
  }

  async componentDidMount() {
    try {
      const response = await fetch(
        url,
        {
          method: 'POST',
          credentials: 'include',
          body: JSON.stringify({
            query,
          })
        }
      );
      if (!response.error) {
        this.setState({ data: response.data, loading: false, error: null});
      } else {
        throw new Error(response);
      }
    } catch (e) {
      this.setState({
        error: e,
        loading: false,
        data: null,
      });
    }
  }

  render() {
    const { data, error, loading } = this.state;
    if (loading) { return <Loading /> }
    if (error) { return <Error error={error} /> }
    return <RenderData data={data} />
  }
}
```

### With Apollo

```js
const query = `query { rootField { field1 field2 }}`

const Comp = () => (
  <Query query={query} />
    {
      ({ data, error, loading}) => {
        if (loading) { return <Loading /> }
        if (error) { return <Error error={error} /> }
        return <RenderData data={data} />    
      }
    } 
  </Query>
);
```

The above two examples make the difference absolutely clear.

## Manipulating the cache

Apollo client also gives us direct access to the cache so that we can use it however we wish.

### Reading

Getting the data from Apollo Cache is a simple `client.readQuery` function. This is a synchronous function, and can be used anywhere.

```js
const data = client.readQuery({
  query: fetchTodos,
  variables: {
    isPublic,
  }
});
```

### Writing

We can also modify the Apollo cache as per our needs. For instance, after inserting a todo, we would like to update the cache to update the UI instantly. There is a `client.writeQuery` function for this purpose:

```js
client.writeQuery({
  query: fetchTodos,
  variables: {
    isPublic
  },
  data: newData
})
```

You must have noticed that we used these functions to update the UI while making [insert](../basic-app/mutation-component-usage-insert.md) and [delete](../basic-app/mutation-component-usage-update-delete) mutations.

## Wrapping up

Next, we will see how to use the cache for the state of your application like `redux` or `this.state` in React.