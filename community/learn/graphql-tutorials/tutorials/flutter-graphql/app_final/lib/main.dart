import 'package:app_final/screens/dashboard.dart';
import 'package:app_final/screens/login.dart';
import 'package:app_final/screens/signup.dart';
import 'package:app_final/screens/splash.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    final routes = <String, WidgetBuilder>{
      "/login": (BuildContext context) => Login(),
      "/dashboard": (BuildContext context) => Dashboard(),
      "/signup": (BuildContext context) => Signup(),
    };
    return MaterialApp(
      title: 'Hasura GraphQL Demo',
      theme: ThemeData(primaryColor: Colors.black),
      routes: routes,
      home: Splash(),
    );
  }
}
