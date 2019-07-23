---
title: "Remove todos - Integration"
metaTitle: "Apollo Mutation Component for GraphQL mutation delete | GraphQL Flutter Tutorial"
metaDescription: "We will use the Mutation Widget with variables as an example to delete existing data"
---

Let us integrate the remove todos feature in our React Native app. Firstly add following mutation string in `lib/data/todo_fetch.dart`.


<!-- TODO - Add git link -->

```dart
+ static String deleteTodo = """mutation delete(\$id:Int!) {
+ action: delete_todos(where: {id: {_eq: \$id}}) {
+    returning {
+      id
+    }
+  }
+ }""";
```

Now, in `TodoItemTile` widget, update the `trailing` widget i.e InkWell and  wrap it in Mutation Widget.

```dart
+  Mutation(
+           options: MutationOptions(document: deleteDocument),
+           builder: (
+             RunMutation runMutation,
+             QueryResult result,
+           ) {
              return InkWell(
               onTap: () {
-                  delete();
+                  runMutation(deleteRunMutaion);
               },
                child: Container(
                    decoration: BoxDecoration(
                        border: Border(left: BorderSide(color: Colors.grey))),
                    width: 60,
                    height: double.infinity,
                    child: Icon(Icons.delete)),
              );
+            },
+           onCompleted: (onValue) {
+             refetchQuery();
+           },
+         )
```
The above code is not using `delete` callback but it is using `deleteDocument`, `deleteRunMutaion` to pass mutation query and mutation varible repectively, So define them in constructor of `TodoItemTile` widget itself and remove `delete` callback function from constructor. Pass your delete mutation query and delete variable document in the widget from screens i.e `all.dart` , `active.dat` , `completed.dart`.

Use same refetchQuery call back in onCompleted function to update UI.

