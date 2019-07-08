---
title: "Run mutation, update cache"
metaTitle: "Apollo Mutation Component | GraphQL React Native Apollo Tutorial"
metaDescription: "We will use the Apollo Client Mutation component from react-apollo in React Native app as an example to insert new data and update cache locally using readQuery and writeQuery."
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/dCm4hIhQB7w" />

Firstly, let us define the mutation that we looked at in the previous section. Import `gql` from `graphql-tag` and define the following mutation in `src/screens/components/Todo/Textbox.js`.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/Textbox.js" text="Textbox.js"/>

```js
+ import gql from 'graphql-tag';

+const INSERT_TODO = gql`
+  mutation ($text: String!, $isPublic: Boolean){
+    insert_todos (
+      objects: [{
+        title: $text,
+        is_public: $isPublic
+      }]
+    ){
+      returning {
+        id
+        title
+        is_completed
+        created_at
+        is_public
+        user {
+          name
+        }
+      }
+    }
+  }
`;

```

Now let's do the integration part. Now add the following code below the other imports:

```javscript
+ import { Mutation } from "react-apollo";
```

We are importing the `Mutation` component from `react-apollo`.

Now, we will wrap the component with `Mutation` passing our graphql mutation constant that we imported. In the render method, add the following code:

```js
render() {

    const { text } = this.state
+   const { isPublic } = this.props;

-    const submit = () => {
-      this.setState({
-        text: ''
-      });
-    }

    return (
+      <Mutation mutation={INSERT_TODO} >
+        {
+          (insertTodo, { loading, error}) => {
            const submit = () => {
              this.setState({
                text: ''
              });
+              insertTodo({
+               variables: { text, isPublic }
+              });
+            }
+            return (
              <View style={styles.inputContainer}>
                <View style={styles.textboxContainer}>
                  <TextInput
                    style={styles.textbox}
                    editable = {true}
                    onChangeText = {this._handleTextChange}
                    value = {text}
                  />
                </View>
                <View style={styles.buttonContainer}>
                  <TouchableOpacity style={styles.button} onPress={submit} disabled={text === ''}>
                    <Text style={styles.buttonText}> Add </Text>
                  </TouchableOpacity>
                </View>
              </View>
+            );
+          }
+        }
+      </Mutation>
    );
}
```

In the `<Mutation>` component defined above, the first argument of the render prop function is the mutate function(insertTodo) in this case. Read more about the mutate function [here](https://www.apollographql.com/docs/react/essentials/mutations.html#render-prop).

The mutate function optionally takes variables, optimisticResponse, refetchQueries, and update; You are going to make use of the `update` function later.

We are passing the mutate function (`insertTodo`) to our Button's `onPress` handler. We are also passing the query variables that is `text` and `isPublic` to this mutate function so that our mutation is called with those variables. `text` is the value of the input box while `isPublic`, which is the type of the todo, is taken from props.

The mutation has been integrated and the new todos will be inserted into the database. But the UI doesn't know that a new todo has been added. We need a way to tell Apollo Client to update the query for the list of todos.

The `update` function comes in handy to update the cache for this mutation. It comes with utility functions such as `readQuery` and `writeQuery` that helps in reading from and writing to the cache.

Let's implement `update` for the above mutation.

We pass the update function as a prop.

```javascript
-    <Mutation mutation={INSERT_TODO}>
+    <Mutation mutation={INSERT_TODO} update={updateCache}>
```

We need to fetch the current list of todos from the cache. So let's import the query that we used in the previous steps.

```javascript
import {FETCH_TODOS} from './Todos';
```

Let's define the updateCache function to read and write to cache. Define the `updateCache` function inside the render function.

```javascript

render() {
  const { text } = this.state
  const { isPublic } = this.props;

+  const updateCache = (cache, {data: {insert_todos}}) => {
+    const data = cache.readQuery({
+      query: FETCH_TODOS,
+      variables: {
+        isPublic,
+      }
+    });
+    const newTodo = insert_todos.returning[0];
+    const newData = {
+      todos: [ newTodo, ...data.todos]
+    }
+    cache.writeQuery({
+      query: FETCH_TODOS,
+      variables: {
+        isPublic,
+      },
+      data: newData
+    });
+  }

}

```

Let's dissect what's happening in this code snippet.

Our goals were simple:

- Make a mutation to insert the new todo in the database.
- Once the mutation is done, we need to update the cache to update the UI.

The update function is used to update the cache after a mutation occurs.
It receives the result of the mutation (data) and the current cache (store) as arguments. You will then use these arguments to manage your cache so that the UI will be up to date.

cache.readQuery
---------------

Unlike `client.query`, readQuery will never make a request to your GraphQL server. It will always read from the cache. So we make a read request to the cache to get the current list of todos.

cache.writeQuery
----------------

We have already done the mutation to the graphql server using the mutate function. Our goal was to update the UI. This is where writeQuery comes to the rescue. writeQuery will allow you to change data in your local cache, but it is important to remember that they will not change any data on your server (exactly what we need).

  Any subscriber to the Apollo Client store will instantly see this update and render new UI accordingly.

We concatenate our new todo from our mutation with the list of existing todos and write the query back to the cache with cache.writeQuery

Now, the `Todos` component wrapped with the `Query` component will get the updated todo list as it is automatically subscribed to the store.

Great! That was actually easy :)

