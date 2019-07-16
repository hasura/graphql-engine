import 'package:app_boilerplate/components/add_task.dart';
import 'package:app_boilerplate/components/todo_item_tile.dart';
import 'package:app_boilerplate/data/todo_list.dart';
import 'package:flutter/material.dart';

class Completed extends StatefulWidget {
  const Completed({Key key}) : super(key: key);

  @override
  _CompletedState createState() => _CompletedState();
}

class _CompletedState extends State<Completed> {
  @override
  Widget build(BuildContext context) {
    return Column(
      children: <Widget>[
        AddTask(
          onAdd: (value) {
            todoList.addTodo(value);
          },
        ),
        Expanded(
          child: ListView.builder(
            itemCount: todoList.completeList.length,
            itemBuilder: (context, index) {
              return TodoItemTile(
                item: todoList.completeList[index],
                delete: () {
                  setState(() {
                    todoList.removeTodo(todoList.completeList[index].id);
                  });
                },
                toggleIsCompleted: () {
                  setState(() {
                    todoList.toggleList(todoList.completeList[index].id);
                  });
                },
              );
            },
          ),
        ),
      ],
    );
  }
}
