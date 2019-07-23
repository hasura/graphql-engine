import 'package:app_boilerplate/data/online_list.dart';
import 'package:flutter/material.dart';

class Online extends StatelessWidget {
  const Online({Key key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Column(
      children: <Widget>[
        Padding(
          padding: const EdgeInsets.all(12.0),
          child: Text(
            "Online Users",
            style: TextStyle(fontSize: 28),
          ),
        ),
        Expanded(
          child: ListView.builder(
            itemCount: onlineList.list.length,
            itemBuilder: (context, index) {
              return Card(
                child: ListTile(
                  title: Text(onlineList.list[index]),
                ),
              );
            },
          ),
        ),
      ],
    );
  }
}
