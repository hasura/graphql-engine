# Create a query and render results

In the React Native app, the todos are rendered by the `Todos` component in `src/screens/components/Todos.js` based on the prop `isPublic` and other `session` props. Currently, it just renders some dummy data. Let us go ahead and integrate this component with the GraphQL backend using the `Query` component.


1. Write the query and wrap it `gql`.

  
    > `gql` is a function from the `graphql-tag` library that converts a GraphQL query string into a standard GraphQL AST.

    ```js
    import gql from 'graphql-tag'; 

    export const fetchTodos = gql`
      query (
        $isPublic: Boolean,
      ){
        todos (
          order_by: id_desc,
          where: { is_public: { _eq: $isPublic} }
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


   In this query, we are fetching the todos with variable `lastId` so that we do not fetch all the data at once. We are also taking `isPublic` from variables so that we can reuse the same query for both public and private todos.


2. Import `Query` component from `react-apollo`:

    ```js
    import { Query } from 'react-apollo';
    ```

3. In this app we are showing a spinner when the query is `loading` and we show an error text when the component is in `error`. You can have custom components to handle both these states. Import the `CenterSpinner` component to wrap it up.

    ```js
    import CenterSpinner from './CenterSpinner'
    ```

4. Render the todos using a `<Query>` component. You can do that by modifying the render method to use the data from the `Query` component.


    ```js
    render() {
      const { isPublic } = this.props;
      return (
        <Query
          query={fetchTodos}
          variables={{isPublic, offset: 0, limit: 10}}
        >
          {
            ({data, error, loading}) => {
              if (error) {
                return <Text>Error</Text>;
              }
              if (loading) {
                return <CenterSpinner />;
              }
              return (
                <View style={styles.container}>
                  <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
                    <FlatList
                      data={data.todos}
                      renderItem={({item}) => <TodoItem item={item} isPublic={this.props.isPublic}/>}
                      keyExtractor={(item) => item.id.toString()}
                    />
                  </ScrollView>
                </View>
              );
            }
          }
        </Query>
      );
    }
    ```


## Wrapping up

If you did everything correctly, the UI should now be rendering the todos from the database and not the dummy data.

Next we will see how the Apollo `<Mutation>` component works and then we will try to use this `<Mutation>` component to enable adding todos from the app.