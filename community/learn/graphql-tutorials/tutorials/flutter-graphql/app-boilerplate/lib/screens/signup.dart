import 'package:app_boilerplate/components/custom_button.dart';
import 'package:app_boilerplate/components/utils.dart';
import 'package:app_boilerplate/services/auth.dart';
import 'package:flutter/material.dart';

class Signup extends StatefulWidget {
  Signup({Key key}) : super(key: key);

  _SignupState createState() => _SignupState();
}

class _SignupState extends State<Signup> {
  TextEditingController emailController = TextEditingController();
  TextEditingController passwordController = TextEditingController();
  final _formKey = GlobalKey<FormState>();
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
                          if (await hasuraAuth.signup(
                            emailController.text,
                            passwordController.text,
                          )) {
                            UtilFs.showToast("SignUp Successful", context);
                            Navigator.pop(context);
                          } else {
                            FocusScope.of(context)
                                .requestFocus(new FocusNode());
                            UtilFs.showToast("SignUp Failed", context);
                          }
                          setState(() {
                            busyView = false;
                          });
                        }
                      },
                      text: "SignUp",
                    ),
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
