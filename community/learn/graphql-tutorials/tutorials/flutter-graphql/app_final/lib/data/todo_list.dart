import 'package:app_final/model/todo_item.dart';

class TodoList {
  List<TodoItem> list = [];
  int id = 0;
  addTodo(String task) {
    id++;
    list.add(
      TodoItem.fromElements(
        id,
        task,
        false,
      ),
    );
  }

  removeTodo(int id) {
    list.removeWhere((item) => item.id == id);
  }

  toggleList(int id) {
    int index = list.indexWhere((item) => item.id == id);
    list[index].isCompleted = !list[index].isCompleted;
  }

  List<TodoItem> get activeList =>
      list.where((item) => item.isCompleted == false).toList();
  List<TodoItem> get completeList =>
      list.where((item) => item.isCompleted == true).toList();
}

TodoList todoList = new TodoList();
