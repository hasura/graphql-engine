---
title: "Query Widget"
metaTitle: "Flutter Query Widget | GraphQL Flutter Tutorial"
metaDescription: "We will use the GraphQl Client Query widget provided by  graphql_flutter package. It is a widget to fetch, handle data and build UI"
---

In this section, we will implement GraphQL Queries and integrate it with the UI.

For queries, graphql_flutter package provides a widget named `Query`.
The `Query` widget provides two parameters, options and builder. `options` takes a widget argument of type `QueryOptions`, which again provides us several parameters like `document` to pass query ,`variables` to pass query variables and `pollInterval` to add polling in the query. `builder` parameter is used to build widgets according to the data received from the query result.

Great! Now let's define the GraphQL query to be used:

Open `lib/data/` and add the file named `todo_fetch.dart`and add the following code:

<!-- TODO github link required  -->

```dart
class TodoFetch {
  static String fetchAll = """query getMyTodos {
  todos(where: { is_public: { _eq: false} },
   order_by: { created_at: desc }) {
    __typename
    id
    title
    is_completed
  }
""";
}
```

We have now written the GraphQL query as a String named fetchAll and it will be passed to QueryOptions as `document` parameter.

## What does this query do?

The query fetches `todos` with a simple condition : `is_public` which must be false. We sort the todos in descending order using its `created_at` attribute, according to the schema. We specify the fields we need for todos node.

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) out this query now!

## Introducing query variables

As you see, we have explicitly mentioned that `is_public` must be false. But in order to reuse this query for private and public todos, we must parameterize this query using `query variables`. Lets define a boolean query variable called `is_public`. The GraphQL query would fetch public todos if `is_public` is true and personal todos if `is_public` is false, which in code is represented as follows:

```graphql
query
 - query getMyTodos {
 + getTodos(\$is_public: Boolean!)
  {
 - todos(where: { is_public: { _eq: false} },
 + todos(where: { is_public: { _eq: \$is_public} },
   order_by: { created_at: desc }) {
    __typename
    id
    title
    is_completed
  }
}
```

Cool! The query is now ready, let's integrate it. Currently, we are just using some dummy data. Let us remove this dummy data and create the UI based on our GraphQL response.

Now, inside `lib/screens/tabs/todos/all.dart` , lets wrap our ListView with Query widget

```dart
Expanded(
+  child: Query(
+           options: QueryOptions(
+             document: TodoFetch.fetchAll,
+             variables: {"is_public": false},
+           ),
+           builder: (QueryResult result, {VoidCallback refetch}) {
+             refetchQuery = refetch;
+             if (result.errors != null) {
+               return Text(result.errors.toString());
+             }
+             if (result.loading) {
+               return Text('Loading');
+             }
+             final List<LazyCacheMap> todos =
+                 (result.data['todos'] as List<dynamic>).cast<LazyCacheMap>();
              return ListView.builder(
-               itemCount: todoList.list.length,
+               itemCount: todos.length,
                itemBuilder: (context, index) {
+                 dynamic responseData = todos[index];
                  return TodoItemTile(
-                   item: todoList.list[index],
+                   item: TodoItem.fromElements(responseData["id"],
+                       responseData['title'], responseData['is_completed'])
-                   delete: () {
-                      setState(() {
-                       todoList.removeTodo(todoList.list[index].id);
-                     });
                   },
                    toggleIsCompleted: () {
-                     setState(() {
-                       todoList.toggleList(todoList.list[index].id);
-                     });
                    },
                  );
                },
          );,
+       }
+      ),
```

NOTE : We will be using `refetchQuery` variable afterwords. So add `VoidCallback refetchQuery;` above build method.

`delete` and `toggleIsCompleted` will not work now as it depends on our local list. So let's leave them empty for now.

Woot! You have written your first GraphQL integration with Flutter. Easy isn't it?

## How does this work?

When you wrapped your widget with `Query` widget, It provides a builder method which gives you result as `QueryResult` object which has several variables to use. Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`errors`: A runtime error with GraphQLErrors and NetworkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.

Using the `data`, we are parsing the results from the server. In our query, `data` has an array `todos` which can be mapped over to create `TodoItem` object which then can be used to create `TodoItemTile` widget.

If you have noted, there has been some client side filtering to the todos that are displayed. For example to collect all active todo or completed todo. For that follow the same procedure and add following queries to your `lib/data/todo_fetch.dart` file.

```dart
static String fetchActive = """query getActiveTodos{
  todos(where: {is_public: {_eq: false}, is_completed: {_eq: false}}, order_by: {created_at: desc}) {
    __typename
    is_completed
    id
    title
  }
  }""";
  static String fetchCompleted = """query getCompletedTodos{
  todos(where: {is_public: {_eq: false}, is_completed: {_eq: true}}, order_by: {created_at: desc}) {
    __typename
    is_completed
    id
    title
  }
  }""";
```
