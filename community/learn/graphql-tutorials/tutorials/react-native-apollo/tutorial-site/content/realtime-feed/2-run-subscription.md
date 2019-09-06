---
title: "Detect new todos - integration"
metaTitle: "Fetch public todos using Subscription | GraphQL React Native Apollo Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in React app. We use withApollo HOC"
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/NPF19xKjfqI" />

Go to `src/screens/components/Todos`.

For running a custom subscription, we need access to our `ApolloClient` instance. To do that, we convert our `Todos` component into an Apollo HOC by wrapping it in `withApollo`.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/Todos.js" text="Todos.js"/>

```js
+ import { withApollo } from 'react-apollo'

- export class Todos extends React.Component {
+ class Todos extends React.Component {
}

- export default Todos;
+ export default withApollo(Todos);
```

Once the component is an Apollo HOC, you receive the instance of Apollo client as a prop called `client.

Now let us define our subscription at the top level:

```js
+const SUBSCRIBE_TO_NEW_TODOS = gql`
+subscription {
+  todos (
+    order_by: {
+      created_at: desc
+    }
+    limit: 1
+    where: { is_public: { _eq: true }}
+  ) {
+    id
+    created_at
+  }
+}
`;
```

Using the above subscription, lets write a class level function called `subscribeToNewTodos` that subscribes to the last public todo in the database. We will start this subscription in the `componentDidMount` of the `Todos` component:

```js
export default class Todos extends React.Component {

  constructor (props) {
    super(props);
+    this.state = {
+      newTodosExist: false
+    }
  }

+  subscribeToNewTodos = () => {
+    const { client, isPublic } = this.props;
+    if (isPublic) {
+      client.subscribe({
+        query: SUBSCRIBE_TO_NEW_TODOS,
+      }).subscribe({
+        next: (event) => {
+          if (event.data.todos.length) {
+            let localData;
+            try {
+              localData = client.readQuery({
+                query: FETCH_TODOS,
+                variables: {
+                  isPublic: true,
+                }
+              });
+            } catch (e) {
+              return;
+            } 
+            
+            const lastId = localData.todos[0] ? localData.todos[0].id : 0;
+            if (event.data.todos[0].id > lastId) {
+              this.setState({ newTodosExist: true})
+            }
+          }
+        },
+        error: (err) => {
+          console.error("err", err);
+        }
+      })
+    }
+  }

+  componentDidMount() {
+    this.subscribeToNewTodos();
+  }

```

The `subscribeToNewTodos` does the following:

1. Starts a subscription to the last todo in the database
2. Whenever data is received, it looks at the todos in Apollo cache and checks if the todo received via subscription is newer than the newest todo in the cache.
3. If the todo received in subscription data has `id` greater than the `id` of the newest todo in the cache, it sets a local state saying `{ newTodosExist: true }`

If new todos exist, we wish to show a button, pressing which, new todos will be loaded. So edit the JSX like:

```js
  <View style={styles.container}>
-    <LoadNewer show={isPublic} styles={styles} isPublic={this.props.isPublic}/>
+    <LoadNewer show={this.state.newTodosExist && isPublic} styles={styles} isPublic={this.props.isPublic}/>
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
```

If you check the app now, this button would be shown only if there are todos in the database newer than the local todos.

We also need to write a function to dismiss this button (when the new todos are loaded), just write a class level function in the `Todos` component and pass it to the button.


```js

componentDidMount() {
  ...
}

dismissNewTodoBanner() {
  this.setState({
    newTodosExist: false
  })
}

render() {
  ...
}

```


```js
  <View style={styles.container}>
-    <LoadNewer show={this.state.newTodosExist && isPublic} styles={styles} isPublic={this.props.isPublic}/>
+    <LoadNewer show={this.state.newTodosExist && isPublic} toggleShow={this.dismissNewTodoBanner} styles={styles} isPublic={this.props.isPublic}/>
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
```

Awesome! You are now detecting new todo updates from the backend.

Now lets make this button functional.