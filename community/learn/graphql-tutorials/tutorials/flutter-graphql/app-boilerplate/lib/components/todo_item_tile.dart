import 'package:app_boilerplate/model/todo_item.dart';
import 'package:flutter/material.dart';

class TodoItemTile extends StatelessWidget {
  final TodoItem item;
  final Function delete;
  final Function toggleIsCompleted;
  TodoItemTile({
    Key key,
    @required this.item,
    @required this.delete,
    @required this.toggleIsCompleted,
  }) : super(key: key);
  @override
  Widget build(BuildContext context) {
    return Container(
      child: Card(
        child: ListTile(
          contentPadding: EdgeInsets.all(0),
          title: Text(item.task,
              style: TextStyle(
                  decoration: item.isCompleted
                      ? TextDecoration.lineThrough
                      : TextDecoration.none)),
          leading: InkWell(
            onTap: () {
              toggleIsCompleted();
            },
            child: Container(
              height: double.infinity,
              padding: const EdgeInsets.symmetric(horizontal: 12.0),
              child: Icon(!item.isCompleted
                  ? Icons.radio_button_unchecked
                  : Icons.radio_button_checked),
            ),
          ),
          trailing: InkWell(
            onTap: () {
              delete();
            },
            child: Container(
                decoration: BoxDecoration(
                    border: Border(left: BorderSide(color: Colors.grey))),
                width: 60,
                height: double.infinity,
                child: Icon(Icons.delete)),
          ),
        ),
      ),
    );
  }
}
