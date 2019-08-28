---
title: "Run mutation, update cache"
metaTitle: "GraphQL Mutataion Widget | GraphQL Flutter Tutorial"
metaDescription: "We will use the GraphQL Mutation widget as an example to insert new data."
---


Firstly, let us define the mutation that we looked at in the previous section. Define the following mutation in `lib/data/todo_fetch.dart`.

<!-- TODO github link required -->

```dart
+ static String addTodo =
+      '''mutation addTodo(\$title: String!, \$isPublic: Boolean!) {
+ action: insert_todos(objects: { title: \$title, is_public: \$isPublic }) {
+    returning {
+     id
+     title
+     is_completed
+   }
+ }
+ }''';

```

Now let's do the integration part. 

We will wrap the AddTask widget with `Mutation` widget passing our graphql mutation string in Mutation Option as a document.

```dart
+ Mutation(
+         options: MutationOptions(document: TodoFetch.addTodo),
+         builder: (
+           RunMutation runMutation,
+           QueryResult result,
+         ) {
+           todoList.addTodo(value);
            return AddTask(
              onAdd: (value) {
-               todoList.addTodo(value);
+               runMutation({'title': value, 'isPublic': false});
              },
            );
+         },
+         onCompleted: (dynamic resultData) {
+           refetchQuery();
+         },
+       ),
```

The mutation widget optionally takes various callbacks like `onCompleted` and `update`.

We are calling the runMutation function  to our Button's `onAdd` callback. We are also passing the query variables that is `title` and `isPublic` to this runMutation function so that our mutation is called with those variables. `title` is the value of the textfeild while `isPublic` is the type of the todo, and we are explicitly marking it as false as it's a private todo.

The mutation has been integrated and the new todos will be inserted into the database. But the UI doesn't know that a new todo has been added. We need a way to tell GraphQL client to refetch the query to update list of todos.

The `onCompleted` callback function comes in handy to update the cache for this mutation.So we will call `refetchQuery` in onCompleted callback.

Great! That was actually easy :)

