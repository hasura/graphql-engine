---
title: "Sync new todos"
metaTitle: "Sync new todos in public feed | GraphQL React Native Apollo Tutorial"
metaDescription: "You will learn how to sync new todos added by other people in the public feed by fetching older and newer data using GraphQL Queries"
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/mQaYuHjUyIg" />

In the previous section we made a button that shows up only when there are new public todos in the database. Now lets make this button functional i.e. on pressing this button, newer todos should be fetched from the backend, synced with the local todos and the button must be dismissed.

Go to `src/screens/components/Todo/LoadNewer.js`, import `gql` and define the query to fetch newer todos.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/LoadNewer.js" text="LoadNewer.js"/>

```js
+ import gql from 'graphql-tag';

+ const FETCH_NEW_TODOS = gql`
+ query ($lastId: Int){
+  todos (
+    order_by: {
+      id: desc
+    },
+    where: {
+      _and: {
+        is_public: { _eq: true},
+        id: { _gt: $lastId}
+      }
+    }
+  ) {
+    id
+    title
+    is_completed
+    created_at
+    is_public
+    user {
+      name
+    }
+  }
+}
+`;
```

Also, import the `FETCH_TODOS` query so that we can read its cache locally.

```js
+ import { FETCH_TODOS } from './Todos';
```

Now, whenever the button is pressed, we wish to make the `FETCH_NEW_TODOS` query and add the data to cache. Lets write a function that does just that.

```js
class LoadNewerButton extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      buttonText: 'New tasks have arrived!',
      loading: false,
    };
  }

+  fetchNewerTodos = async () => {
+    const { client, isPublic } = this.props;
+    const data = client.readQuery({
+      query: FETCH_TODOS,
+      variables: {
+        isPublic,
+      }
+    });
+    const lastId = data.todos[0].id;
+    this.setState({ loading: true})
+    const resp = await client.query({
+      query: FETCH_NEW_TODOS,
+      variables: { lastId }
+    });
+    this.setState({ loading: false})
+    if (resp.data) {
+      const newData = {
+        todos: [ ...resp.data.todos, ...data.todos]
+      }
+      client.writeQuery({
+        query: FETCH_TODOS,
+        variables: {
+          isPublic,
+        },
+        data: newData
+      });
+      this.props.toggleShow();
+    }
+  }
  
  render () {
    const { disabled, buttonText, loading } = this.state;
    const { styles, show } = this.props;
    if (!show) {
      return null;
    }
    return (
      <TouchableOpacity
        style={styles.pagination}
+        onPress={this.fetchNewerTodos}
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
    )
  }

}
```

With this, your fully functional realtime todo app is ready.
