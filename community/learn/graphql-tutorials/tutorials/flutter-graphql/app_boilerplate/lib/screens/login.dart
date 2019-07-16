import 'package:app_boilerplate/components/custom_button.dart';
import 'package:app_boilerplate/components/utils.dart';
import 'package:app_boilerplate/services/auth.dart';
import 'package:app_boilerplate/services/shared_preferences_service.dart';
import 'package:flutter/material.dart';

class Login extends StatefulWidget {
  Login({Key key}) : super(key: key);

  _LoginState createState() => _LoginState();
}

class _LoginState extends State<Login> {
  TextEditingController emailController = TextEditingController();
  TextEditingController passwordController = TextEditingController();
  final _formKey = GlobalKey<FormState>();
  String _token = "";
  bool busyView = false;

  @override
  Widget build(BuildContext context) {
    if (!busyView) {
      return Scaffold(
        body: Center(
          child: Form(
            key: _formKey,
            child: Column(
              mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              children: <Widget>[
                Text(
                  "ToDo App",
                  style: TextStyle(fontWeight: FontWeight.w800, fontSize: 38),
                ),
                Container(
                  width: MediaQuery.of(context).size.width / 1.3,
                  child: Column(
                    children: <Widget>[
                      TextFormField(
                        controller: emailController,
                        decoration: InputDecoration(labelText: "Email"),
                        keyboardType: TextInputType.emailAddress,
                        validator: (value) {
                          Pattern pattern =
                              r'^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$';
                          RegExp regex = new RegExp(pattern);
                          if (!regex.hasMatch(value))
                            return 'Enter Valid Email';
                          else
                            return null;
                        },
                      ),
                      TextFormField(
                        controller: passwordController,
                        decoration: InputDecoration(labelText: "Password"),
                        validator: (value) {
                          return value.length < 4
                              ? "Password must be at least 4 characters long"
                              : null;
                        },
                        obscureText: true,
                      ),
                    ],
                  ),
                ),
                Column(
                  children: <Widget>[
                    CustomButton(
                      width: 180,
                      height: 50,
                      onTap: () async {
                        if (_formKey.currentState.validate()) {
                          setState(() {
                            busyView = true;
                          });
                          _token = await hasuraAuth.login(
                            emailController.text,
                            passwordController.text,
                          );
                          if (_token != null) {
                            UtilFs.showToast("Login Successful", context);
                            await sharedPreferenceService.setToken(_token);
                            Navigator.pushReplacementNamed(
                                context, "/dashboard");
                          } else {
                            setState(() {
                              busyView = false;
                            });
                            UtilFs.showToast("Login Failed", context);
                            FocusScope.of(context)
                                .requestFocus(new FocusNode());
                          }
                        }
                      },
                      text: "Login",
                    ),
                    Padding(
                      padding: const EdgeInsets.all(18.0),
                      child: GestureDetector(
                        onTap: () {
                          Navigator.pushNamed(context, "/signup");
                        },
                        child: Container(
                          height: 20,
                          child: Text(
                            "New? Sign Up",
                            style:
                                TextStyle(decoration: TextDecoration.underline),
                          ),
                        ),
                      ),
                    )
                  ],
                )
              ],
            ),
          ),
        ),
      );
    } else {
      return Scaffold(
        body: Center(
          child: CircularProgressIndicator(),
        ),
      );
    }
  }
}
