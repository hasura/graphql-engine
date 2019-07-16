import 'package:app_final/screens/tabs/todos/active.dart';
import 'package:app_final/screens/tabs/todos/all.dart';
import 'package:app_final/screens/tabs/todos/completed.dart';
import 'package:flutter/material.dart';

class Todos extends StatefulWidget {
  Todos({Key key}) : super(key: key);

  _TodosState createState() => _TodosState();
}

class _TodosState extends State<Todos> {
  @override
  Widget build(BuildContext context) {
    return DefaultTabController(
      length: 3,
      child: Scaffold(
        appBar: AppBar(
          title: TabBar(
            tabs: [
              Tab(
                text: "All",
              ),
              Tab(
                text: "Active",
              ),
              Tab(
                text: "Completed",
              ),
            ],
          ),
        ),
        body: TabBarView(
          children: [
            All(),
            Active(),
            Completed(),
          ],
        ),
      ),
    );
  }
}
