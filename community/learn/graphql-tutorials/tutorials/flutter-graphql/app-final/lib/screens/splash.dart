import 'package:app_final/services/shared_preferences_service.dart';
import 'package:flutter/material.dart';

class Splash extends StatelessWidget {
  const Splash({
    Key key,
  }) : super(key: key);

  initMethod(context) async {
    await sharedPreferenceService.getSharedPreferencesInstance();
    String _token = await sharedPreferenceService.token;
    if (_token == null || _token == "") {
      Navigator.of(context).pushReplacementNamed('/login');
    } else
      Navigator.of(context).pushReplacementNamed('/dashboard');
  }

  @override
  Widget build(BuildContext context) {
    WidgetsBinding.instance.addPostFrameCallback((_) => initMethod(context));

    return Scaffold(
      body: Center(
        child: new CircularProgressIndicator(),
      ),
    );
  }
}
