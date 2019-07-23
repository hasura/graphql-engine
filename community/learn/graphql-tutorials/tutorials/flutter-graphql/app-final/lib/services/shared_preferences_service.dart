import 'package:shared_preferences/shared_preferences.dart';

class SharedPreferenceService {
  SharedPreferences _prefs;

  Future<bool> getSharedPreferencesInstance() async {
    _prefs = await SharedPreferences.getInstance().catchError((e) {
      print("shared prefrences error : $e");
      return false;
    });
    return true;
  }

  Future setToken(String token) async {
    await _prefs.setString('token', token);
  }

  Future clearToken() async {
    await _prefs.clear();
  }

  Future<String> get token async => _prefs.getString('token');
}

SharedPreferenceService sharedPreferenceService = SharedPreferenceService();
