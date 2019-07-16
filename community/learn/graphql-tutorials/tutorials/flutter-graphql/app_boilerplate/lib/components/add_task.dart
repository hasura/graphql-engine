import 'package:app_boilerplate/components/custom_button.dart';

import 'package:flutter/material.dart';

class AddTask extends StatelessWidget {
  //final TodoList todoList;
  final Function onAdd;
  const AddTask({Key key, this.onAdd}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    TextEditingController _controller = TextEditingController();
    return Container(
      child: Padding(
        padding: const EdgeInsets.all(18.0),
        child: Row(
          mainAxisSize: MainAxisSize.min,
          children: <Widget>[
            Expanded(
              child: TextFormField(
                controller: _controller,
                decoration: InputDecoration(
                  labelText: "Add task",
                  border: OutlineInputBorder(),
                ),
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(8.0),
              child: CustomButton(
                width: 90,
                height: 50,
                onTap: () {
                  onAdd(_controller.text);
                  _controller.clear();
                  FocusScope.of(context).requestFocus(new FocusNode());
                },
                text: "Add",
              ),
            )
          ],
        ),
      ),
    );
  }
}
