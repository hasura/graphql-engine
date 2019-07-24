---
title: "Handle loading/errorrs"
metaTitle: "GraphQL Client Error Handling | GraphQL Flutter Tutorial"
metaDescription: "We will handle the GraphQL loading and error Flutter app"
---


As we saw in the previous step, we got various variables in `QueryResult` object. Among them `loading` and `errors` are common ones that you will need to handle in your app.

Now let's go back to the `Query` widget that you wrote in the previous step.


```dart

    Query(
            options: QueryOptions(
              document: TodoFetch.fetchAll,
              variables: {"is_public": false},
            ),
            builder: (QueryResult result, {VoidCallback refetch}) {
              refetchQuery = refetch;
              if (result.errors != null) {
                return Text(result.errors.toString());
              }
              if (result.loading) {
                return Text('Loading');
              }
              final List<LazyCacheMap> todos =
                  (result.data['todos'] as List<dynamic>).cast<LazyCacheMap>();
              return ListView.builder(
                itemCount: todos.length,
                itemBuilder: (context, index) {
                  dynamic responseData = todos[index];
                  return TodoItemTile(
                    item: TodoItem.fromElements(responseData["id"],
                        responseData['title'], responseData['is_completed']),
                    toggleDocument: TodoFetch.toggleTodo,
                    toggleRunMutaion: {
                      'id': responseData["id"],
                      'isCompleted': !responseData['is_completed']
                    },
                    deleteDocument: TodoFetch.deleteTodo,
                    deleteRunMutaion: {
                      'id': responseData["id"],
                    },
                    refetchQuery: refetch,
                  );
                },
              );
            },
          )


```


When Query's `builder` runs you can manage states of your app like loading and error.  
In loading state, typically you can do fancy things like displaying a loading spinner.

We are just printing `error` but you can handel it also, by navigating to some screen or showing some pop up. 

All said and done, What you have written above is basic, but sufficient for this tutorial.
