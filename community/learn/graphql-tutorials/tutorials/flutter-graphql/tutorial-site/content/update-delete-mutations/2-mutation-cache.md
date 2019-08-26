---
title: "Update mutation and automatic cache updates"
metaTitle: "GraphQL Mutation widget for GraphQL mutation update | GraphQL Flutter Tutorial"
metaDescription: "We will use the Mutation widget from package as an example to modify existing data and update cache automatically and handle optimisticResponse"
---


Now let's do the integration part. Open `lib/data/todo_fetch` and add the following code.

```dart
+ static String toggleTodo =
+      """mutation toggleTodo(\$id:Int!, \$isCompleted: Boolean!) {
+  action: update_todos(where: {id: {_eq: \$id}}, _set: {is_completed: \$isCompleted}) {
+   returning {
+      is_completed
+    }
+  }
+ }""";
```

Now, in `TodoItemTile` widget inside `lib/component/todo_item_tile.dart`, let's modify leading widget i.e Inkwell.

```dart
+   Mutation(
+           options: MutationOptions(document: toggleDocument),
+           builder: (
+             RunMutation runMutation,
+             QueryResult result,
+           ) {
+             return InkWell(
+               onTap: () {
-                 toggleIsCompleted();
+                 runMutation(
+                  toggleRunMutaion,
+                  optimisticResult: {
+                     "action": {
+                       "returning": [
+                         {"is_completed": !item.isCompleted}
+                       ]
+                     }
+                   },
+                 );
+               },
                child: Container(
                  height: double.infinity,
                  padding: const EdgeInsets.symmetric(horizontal: 12.0),
                  child: Icon(!item.isCompleted
                      ? Icons.radio_button_unchecked
                      : Icons.radio_button_checked),
                ),
+             );
+           },
+           update: (Cache cache, QueryResult result) {
+             if (result.hasErrors) {
+               print(result.errors);
+             } else {
+               final Map<String, Object> updated =
+                   Map<String, Object>.from(item.toJson())
+                     ..addAll(extractTodoData(result.data));
+               cache.write(typenameDataIdFromObject(updated), updated);
+             }
+             return cache;
+           },
+           onCompleted: (onValue) {
+             refetchQuery();
+           },
+         )
```
The above code isn't using `toggleIsCompleted` callback but it's using `toggleDocument`, `toggleRunMutaion` to pass mutation query and mutation variable repectively, So define them in constructor of `TodoItemTile` widget itself and remove `toggleIsCompleted` callback function from constructor. Pass your toggle mutation query and toggle variable document in the widget from screens i.e `all.dart` , `active.dart` and `completed.dart`.

`runMutation` has `optimisticResult` as an argument in which we can pass as our optimistic result.
Now we have to update our cache according to optimisticResult. For this, we have to find the key of item in the cache using `typenameDataIdFromObject` function which takes the item of type map as parameter to be updated.
We get that map using `extractTodoData`function which we will define in `TodoItemTile` itself.

Note: We are using refetchQuery inside our onCompleted function which we also get as a callback using constructor itself.

Add this function code in your `TodoItemTile` class(or widget).

```dart
+ Map<String, Object> extractTodoData(Object data) {
+   final Map<String, Object> returning =
+       (data as Map<String, Object>)['action'] as Map<String, Object>;
+   if (returning == null) {
+     return null;
+   }
+   List<Object> list = returning['returning'];
+   return list[0] as Map<String, Object>;
+ }
```
Add finally cache.write will update the cache.

To summarize the above code it follows that :

1. It looks at the `id` and `__typename` of the mutation response.
2. It looks for the objects in the cache that have `id` and `__typename` similar to the ones in the mutation response.
3. If there is a match, it updates the cache with the data from the mutation response.

