---
title: "Handle loading/errorrs"
metaTitle: "Apollo Query Component Error Handling | GraphQL React Native Apollo Tutorial"
metaDescription: "We will handle the GraphQL loading and error states in React Native app using the Apollo Query Component render props - loading and error "
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/RoZRYf-7mUY" />

As we saw in the previous step, Apollo injected props into the component’s render prop function. Among them `loading` and `error` are common ones that you will need to handle in your app.

Now let's go back to the `<Query>` component that you wrote in the previous step.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/Todos.js" text="Todos.js"/>

```javascript

<Query
  query={FETCH_TODOS}
  variables={{isPublic: this.props.isPublic}}
>
  {
    ({data, error, loading }) => {
      if (error) {
        console.error(error);
        return <Text>Error</Text>;
      }

      if (loading) {
        return <CenterSpinner />;
      }

      return (
        <View style={styles.container}>
        <LoadNewer show={this.state.newTodosExist && isPublic} toggleShow={this.dismissNewTodoBanner} styles={styles} isPublic={this.props.isPublic}/>
          <ScrollView style={styles.scrollView} contentContainerStyle={styles.scrollViewContainer}>
            <FlatList
              data={data.todos}
              renderItem={({item}) => <TodoItem item={item} isPublic={this.props.isPublic}/>}
              keyExtractor={(item) => item.id.toString()}
            />
            <LoadOlder
              isPublic={this.props.isPublic}
              styles={styles}
            />
          </ScrollView>
        </View>
      );
    }
  }
</Query>
```


When this component mounts, the GraphQL query sent in the background may not have been completed. But we need to handle that temporary state of no data and hence we return some useful text during `loading` state. 
In this loading state, typically you can do fancy things like displaying a loading spinner.

Now, the query could also end up in an `error` state due to various reasons. Sometimes the graphql query could be wrong, or the server isn't responding. Whatever may be the reason, the user facing UI should show something to convey that an error has occurred. 
In this error state, typically you can send these error messages to third-party services to track what went wrong.

All said and done, these are two important states that need to be handled inside your component. What you have written above is basic, but sufficient for this tutorial.
