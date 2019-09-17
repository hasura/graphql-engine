---
title: "Remove todos - Integration"
metaTitle: "Mutation widget for GraphQL mutation delete | GraphQL Flutter Tutorial"
metaDescription: "We will use the Mutation Widget with variables as an example to delete existing data"
---

Let us integrate the remove todos feature in our Flutter app. Initially add the following mutation string in `lib/data/todo_fetch.dart`.

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

Now, in `TodoItemTile` widget, update the `trailing` widget i.e InkWell and wrap it in a mutation widget.

```dart
- trailing: Inkwell
+ trailing: Mutation(
+           options: MutationOptions(document: deleteDocument),
+           builder: (
+             RunMutation runMutation,
+             QueryResult result,
+           ) {

+              return InkWell(
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
-              ),
+              );
+            },
+           onCompleted: (onValue) {
+             refetchQuery();
+           },
+         )
```

The above code is not using `delete` callback but it's using `deleteDocument`, `deleteRunMutaion` to pass mutation query and mutation variable repectively,So define them in constructor of `TodoItemTile` widget itself and remove `delete` callback function from the constructor. Pass your delete mutation query and delete variable document in the widget from screens i.e `all.dart` , `active.dart` and `completed.dart`.

```dart
class TodoItemTile extends StatelessWidget {
  final TodoItem item;
- final Function delete;
  final String toggleDocument;
  final Map<String, dynamic> toggleRunMutaion;
+ final String deleteDocument;
+ final Map<String, dynamic> deleteRunMutaion;
  final Function refetchQuery;

  TodoItemTile({
    Key key,
    @required this.item,
-   @required this.delete,
    this.refetchQuery,
    @required this.toggleDocument,
    @required this.toggleRunMutaion,
+   @required this.deleteDocument,
+   @required this.deleteRunMutaion,
  }) : super(key: key);

```

Now in `lib/screens/tabs/todos/all.dart` modify your code accordingly.

```dart
return TodoItemTile(
                      item: TodoItem.fromElements(
                          responseData["id"], responseData['title'], responseData['is_completed']),
-                     delete:(){}  ,
                     toggleDocument: TodoFetch.toggleTodo,
                    toggleRunMutaion: {'id': responseData["id"], 'isCompleted': !responseData['is_completed']},
+                     deleteDocument: TodoFetch.deleteTodo,
+                     deleteRunMutaion: {
+                       'id': responseData["id"],
+                     },
                     refetchQuery: refetchQuery,
                   );

```

Use same refetchQuery call back in onCompleted function to update the UI.
