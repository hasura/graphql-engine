import 'package:app_final/components/add_task.dart';
import 'package:app_final/components/todo_item_tile.dart';
import 'package:app_final/data/todo_fetch.dart';
import 'package:app_final/data/todo_list.dart';
import 'package:app_final/model/todo_item.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';

class Completed extends StatefulWidget {
  const Completed({Key key}) : super(key: key);

  @override
  _CompletedState createState() => _CompletedState();
}

class _CompletedState extends State<Completed> {
  VoidCallback refetchQuery;
  @override
  Widget build(BuildContext context) {
    return Column(
      children: <Widget>[
        Mutation(
          options: MutationOptions(
              document: TodoFetch
                  .addTodo // this is the mutation string you just created
              ),
          builder: (
            RunMutation runMutation,
            QueryResult result,
          ) {
            return AddTask(
              onAdd: (value) {
                runMutation({'title': value, 'isPublic': false});
                todoList.addTodo(value);
              },
            );
          },
          onCompleted: (dynamic resultData) {
            refetchQuery();
          },
        ),
        Expanded(
          child: Query(
            options: QueryOptions(
              document: TodoFetch.fetchCompleted,
            ),
            builder: (QueryResult result, {VoidCallback refetch}) {
              if (result.errors != null) {
                return Text(result.errors.toString());
              }
              if (result.loading) {
                return Text('Loading');
              }
              return ListView.builder(
                itemCount: result.data['todos'].length,
                itemBuilder: (context, index) {
                  refetchQuery = refetch;
                  dynamic responseData = result.data['todos'][index];
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
                    refetchQuery: refetchQuery,
                  );
                },
              );
            },
          ),
        ),
      ],
    );
  }
}
