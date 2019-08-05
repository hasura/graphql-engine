---
title: "Load more todos integration"
metaTitle: "GraphQL Query to load older todos | GraphQL React Native Apollo Tutorial"
metaDescription: "You will learn the GraphQL query to be made to load older todos with parameters and arguments."
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/ajMuxGqX-Dg" />

In the last section, we modified the query that fetches all the todos to fetch only 10 todos on first load. We also wrote another query that fetches older todos. Now let us implement a button that loads older todos on press. Go to `src/screens/components/Todo/LoadOlder.js`, import `gql` from `graphql-tag` and define the query that we wrote in the last section:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/LoadOlder.js" text="LoadOlder.js"/>

```js
+ import gql from 'graphql-tag';


+const FETCH_OLD_TODOS = gql`
+query ($lastId: Int, $isPublic: Boolean){
+  todos (
+    order_by: {
+      id: desc
+    },
+   where: {
+     _and: {
+       is_public: { _eq: $isPublic},
+      id: { _lt: $lastId}
+     }
+   },
+   limit: 10
+ ) {
+   id
+   title
+   is_completed
+   created_at
+   is_public
+   user {
+     name
+   }
+ }
+}
+`
```

Now write a class function in the `LoadOlder` component.

```js
class LoadOlder extends React.Component {
 
  constructor(props) {
    super(props);
    this.state = {
      buttonText: 'Load more todos',
      disabled: false,
      loading: false,
    };
  }

+  fetchOlderTodos = async () => {
+    const { client, isPublic } = this.props;
+    const data = client.readQuery({
+      query: FETCH_TODOS,
+      variables: {
+        isPublic,
+      }
+    });
+    const numTodos = data.todos.length;
+    this.setState({ disabled: true });
+    this.setState({ loading: true });
+    const response = await client.query({
+      query: FETCH_OLD_TODOS,
+      variables: {
+        isPublic,
+        lastId: numTodos === 0 ? 0 : data.todos[numTodos - 1].id
+      },
+    });
+    this.setState({ loading: false });
+    if (!response.data) {
+      this.setState({ disabled: false })
+      return;
+    }
+    if (response.data.todos) {
+      client.writeQuery({
+        query: FETCH_TODOS,
+        variables: {
+          isPublic
+        },
+        data: { todos: [ ...data.todos, ...response.data.todos]}
+      });
+      if (response.data.todos.length < 10) {
+        this.setState({ buttonText: 'No more todos', disabled: true })
+      } else {
+        this.setState({ buttonText: 'Load more todos', disabled: false})
+      }
+    } else {
+      this.setState({ buttonText: 'Load more todos' });  
+    }
+  }


  render() {
    ...
  }
}
```

The `this.fetchOlderTodos` function does the following:

1. Reads the cache to get the data for query `FETCH_TODOS` and store it in a variable called `data`
2. Makes a GraphQL query to get 10 todos older than the oldest todo in the cache
3. Gets data and updates the apollo cache with the newly received todos. All the `Query` components subscribed to this cache are updated after this update
4. Sets an appropriate button text (`No more todos` if less than 10 todos were fetched, or `Load more todos` if `10` todos were fetched).

Finally, integrate this function into the JSX such that it is invoked whenever the button is pressed.

```js
<TouchableOpacity
  style={styles.pagination}
+ onPress={this.fetchOlderTodos}
  disabled={disabled}
> 
  {
    loading ?
    <CenterSpinner /> :
    <Text style={styles.buttonText}>
      {buttonText}
    </Text>
  }
</TouchableOpacity> 
```

The pagination functionality should be working now :)


