# Create a Subscription and render results

In the React Native app, the online users are rendered by the `OnlineUsers` screen in `src/screens/UserScreen.js`. Currently, it just renders some dummy data. Let us go ahead and integrate this component with the GraphQL backend.

We need the User list to update realtime. Therefore we can use a Subscription component


1. Import the required components

    ```js
    import gql from 'graphql-tag';
    import { Subscription } from 'react-apollo';
    import CenterSpinner from './components/CenterSpinner';
    ```

1. Write the subscription query and wrap it in `gql`.

    ```js
    const subscribeToOnlineUsers = gql`
      subscription {
        online_users {
          name
        }
      }
    `;
    ```

    In this subscription, we are fetching the users from the view `online_users`.


2. Render the users using a `Subscription` component. You can do that by modifying the render method to use the data from the `Subscription` component.


    ```js
    render() {
      return (
        <View style={styles.container}>
          <Subscription
            subscription={subscribeToOnlineUsers}
          >
            {
              ({data, loading, error}) => {
                if (loading) { return <CenterSpinner />}
                if (error) {
                  return <Text> Error </Text>
                }
                return (
                  <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
                  <FlatList
                    data={data.online_users}
                    renderItem={({item}) => <UserItem item={item} />}
                    keyExtractor={(item) => item.name}
                  />
                  </ScrollView>
                )
              }
            }
          </Subscription>
        </View>
      );
    }
    ```

3. In this app we are showing a spinner when the query is `loading` and we show an error text when the component is in `error`. You can have custom components to handle both these states.

## Wrapping up

If you did the above correctly, you must be able to see the online users list from the drawer on the left. However, you must have noticed that you can't see your own name in the list. This is because we have not implemented the logic to mark the current user as online. In the next section, we will emit events from the app which will tell other app users that you are online.

