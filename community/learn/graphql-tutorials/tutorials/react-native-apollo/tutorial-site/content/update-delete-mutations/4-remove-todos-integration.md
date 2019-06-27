---
title: "Remove todos - Integration"
metaTitle: "Apollo Mutation Component for GraphQL mutation delete | GraphQL React Native Apollo Tutorial"
metaDescription: "We will use the Apollo Mutation component from react-apollo with variables as an example to delete existing data and update cache automatically"
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/nz26rPS6dLk" />

Let us integrate the remove todos feature in our React Native app. Firstly import `gql` and define the mutation in `src/screens/components/Todo/TodoItem/js`.


<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/TodoItem.js" text="TodoItem.js"/>

```js
+ import gql from 'graphql-tag';

+ const REMOVE_TODO = gql`
+  mutation ($id: Int) {
+    delete_todos (
+      where: {
+        id: {
+          _eq: $id
+        }
+      }
+    ) {
+      affected_rows
+    }
+  }
+`;
```

Now, in the render method of the `TodoItem` component, update the `deletetButton` function to wrap the button JSX with a `Mutation` component.

```js

const deleteButton = () => {
  if (isPublic) return null;
-  const remove = () => {
-    if (loading) { return; }
-  };
  return (
+    <Mutation
+      mutation={DELETE_TODO}
+      update={(cache) => {
+        const data = cache.readQuery({
+          query: FETCH_TODOS,
+          variables: {
+            isPublic,
+          }
+        });
+        const newData = {
+          todos: data.todos.filter((t) => t.id !== item.id)
+        }
+        cache.writeQuery({
+          query: FETCH_TODOS,
+          variables: {
+            isPublic,
+          },
+          data: newData
+        });
+      }}
+    >
+      {
+        (deleteTodo, { loading, error }) => {
+          const remove = () => {
+            if (loading) { return; }
+            deleteTodo({
+              variables: { id: item.id }
+            });
+          };
+          return (
            <View style={styles.deleteButton}>
              <Icon
                name="delete"
                size={26}
                onPress={remove}
                disabled={loading}
                color={loading ? "lightgray" : "#BC0000"}
              />
            </View>
+          );
+        }
+      }
+    </Mutation> 
  )
}
```


This was done similar to the `insert_todos` mutation. We have also updated the cache in the `update` function. With this, we have a fully functional todo app working :)
