---
title: "<Query> component"
metaTitle: "Apollo Query Component | GraphQL React Native Apollo Tutorial"
metaDescription: "We will use the Apollo Client Query component from react-apollo. It is a render prop API to fetch data and handle data, loading and error props"
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/qWcIXZ15FFw" />

In this section, we will implement GraphQL Queries and integrate with the react native UI.
With Apollo Client, you can send queries in 2 different ways.

1. Using the `query` method directly and then process the response.
2. Render Prop API. (Recommended)

The recommended method is to use the render prop method, where you will just pass your GraphQL query as prop and `<Query />` component will fetch the data automatically and will present it in the component's render prop function.

Great! Now let's define the graphql query to be used:

Open `src/screens/components/Todo/Todos.js` and add the following code:


<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/Todos.js" text="Todos.js" />

```javascript
import React from 'react';
import {
  ScrollView,
  StyleSheet,
  Text,
  View,
  FlatList,
} from 'react-native';
+ import gql from 'graphql-tag';
import TodoItem from './TodoItem';
import LoadOlder from './LoadOlder';
import LoadNewer from './LoadNewer';
import CenterSpinner from '../Util/CenterSpinner';

+ export const FETCH_TODOS = gql`
+ query {
+   todos (
+     order_by: {
+       created_at: desc
+     },
+     where: { is_public: { _eq: false} }
+   ) {
+     id
+     title
+     is_completed
+     created_at
+     is_public
+     user {
+       name
+     }
+   }
+ }
`;
```

We have now written the graphql query as a javascript constant using the `gql` parser function. This function is used to parse the plain string as a graphql query.

What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) out this query now!

Introducing query variables
---------------------------

As you see, we have explicitly mentioned that `is_public` must be false. But in order to reuse this query for private and public todos, we must parameterise this query using `query variables`. Lets define a boolean query variable called `is_public`. The GraphQL query would fetch public todos if `is_public` is true and personal todos if `is_public` is false. Change it in the code as follows:

```graphql
- query {
+ query ($isPublic: Boolean) {
+   todos (
+     order_by: {
+       created_at: desc
+     },
-     where: { is_public: { _eq: false} }
+     where: { is_public: { _eq: $isPublic} }
+   ) {
+     id
+     title
+     is_completed
+     created_at
+     is_public
+     user {
+       name
+     }
+   }
+ }
```

Great! The query is now ready, let's integrate it with our react native code. Currently, we are just rendering some dummy data. Let us remove this dummy data and render the UI based on our GraphQL response. Firstly, lets import `Query` component from `react-apollo`.

```js

+ import {Query} from 'react-apollo';

```

Now, inside the render method, get GraphQL data using the Query component and render it:


```js
render () {
  const { isPublic } = this.props;
-  const data = {
-    todos: [
-      {
-        id: "1",
-        title: "This is todo 1",
-        is_completed: true,
-        is_public: isPublic,
-        user: {
-          id: "1",
-          name: "user1"
-        }
-      },
-      {
-        id: "2",
-        title: "This is todo 2",
-        is_completed: false,
-        is_public: isPublic,
-        user: {
-          id: "2",
-          name: "user2"
-        }
-      }
-    ]
-  }
  return (
+    <Query
+      query={FETCH_TODOS}
+      variables={{isPublic: this.props.isPublic}}
+    >
+      {
+        ({data, error, loading }) => {
+          if (!data.todos) return null;
+          return (
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
+          );
+        }
+      }
+    </Query>
  );
}
```

Remember that we wrapped our App component with `<ApolloProvider>` and passed `client` as a prop. We are using the same client prop to send it down to the components.

We are importing the `Query` component from `react-apollo` and the graphql query we defined above to fetch the todo data.

Then, we wrap the new functional component with `Query` passing our graphql query. We also provide a prop to this Query component called `variables`. This is where we pass the value of the query variables.

Woot! You have written your first GraphQL integration with React. Easy isn't it?

How does this work?
-------------------
When you wrapped your return with `<Query>` component, Apollo injected props into the component’s render prop function. Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`error`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

You can read more about other render props that Apollo passes [here](https://www.apollographql.com/docs/react/essentials/queries.html#render-prop)

Using the `data` prop, we are parsing the results from the server. In our query, `data` prop has an array `todos` which can be mapped over to render each `TodoItem`.

If you noted, there has been some client side filtering to the todos that are displayed.
