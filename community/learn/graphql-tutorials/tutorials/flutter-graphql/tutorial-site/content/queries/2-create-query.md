---
title: "<Query> Widget"
metaTitle: "Flutter Query Widget | GraphQL Flutter Tutorial"
metaDescription: "We will use the GraphQl Client Query widget provided by  graphql_flutter package. It is a widget to fetch, handle data and build UI"
---

In this section, we will implement GraphQL Queries and integrate it with the UI.

For Queries graphql_flutter package provides a Widget named Query.
The query widget provides two parameters, options and builder. `options` takes `QueryOptions` widget which again provides us several parameters like `document` to pass query ,`variables` to pass query variables `pollInterval` to add polling in query. And builder is to build a Widgets according to the data received from a query result.


Great! Now let's define the graphql query to be used:

Open `lib/data/` and add the file named `todo_fetch.dart`and add following code:

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

We have now written the graphql query as a String named fetchAll and this is passed to QueryOptions as `document` parameter.

What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) out this query now!

Introducing query variables
---------------------------

As you see, we have explicitly mentioned that `is_public` must be false. But in order to reuse this query for private and public todos, we must parameterise this query using `query variables`. Lets define a boolean query variable called `is_public`. The GraphQL query would fetch public todos if `is_public` is true and personal todos if `is_public` is false. Change it in the code as follows:

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

Great! The query is now ready, let's integrate it. Currently, we are just using some dummy data. Let us remove this dummy data and create the UI based on our GraphQL response.

Now, inside `lib/screens/tabs/todos/all.dart` ,  lets wrap our ListView with Query widget

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

As `delete` and `toggleIsCompleted` depends on our local list and will not work now.So let's leave them empty

Woot! You have written your first GraphQL integration with Flutter. Easy isn't it?

How does this work?
-------------------
When you wrapped your widget with `Query` widget, It provides builder method which gives you result as `QueryResult` object which have several variables to use. Most important ones are:

`loading`: A boolean that indicates whether the request is in flight. If loading is true, then the request hasn't finished. Typically this information can be used to display a loading spinner.

`errors`: A runtime error with graphQLErrors and networkError properties. Contains information about what went wrong with your query.

`data`: An object containing the result of your GraphQL query. This will contain our actual data from the server. In our case, it will be the todo data.


Using the `data`, we are parsing the results from the server. In our query, `data`  has an array `todos` which can be mapped over to create `TodoItem` object which then used to create `TodoItemTile` widget.

If you noted, there has been some client side filtering to the todos that are displayed. Like to collect all active todo or completed todo. For that follow the same procedure and add following queries to your `lib/data/todo_fetch.dart` file.

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
