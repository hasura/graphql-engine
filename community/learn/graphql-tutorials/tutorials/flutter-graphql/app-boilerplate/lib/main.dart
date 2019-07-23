import 'package:app_boilerplate/screens/dashboard.dart';
import 'package:app_boilerplate/screens/login.dart';
import 'package:app_boilerplate/screens/signup.dart';
import 'package:app_boilerplate/screens/splash.dart';
import 'package:flutter/material.dart';

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
