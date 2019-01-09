import history from "../../utils/history";
import auth0 from "auth0-js";
import { AUTH_CONFIG } from "./auth0-variables";

export default class Auth {
  auth0 = new auth0.WebAuth({
    domain: AUTH_CONFIG.domain,
    clientID: AUTH_CONFIG.clientId,
    redirectUri: AUTH_CONFIG.callbackUrl,
    audience: `https://${AUTH_CONFIG.domain}/userinfo`,
    responseType: "token id_token",
    scope: "openid profile"
  });
  constructor() {
    this.login = this.login.bind(this);
    this.logout = this.logout.bind(this);
    this.handleAuthentication = this.handleAuthentication.bind(this);
    this.isAuthenticated = this.isAuthenticated.bind(this);
  }

  login() {
    this.auth0.authorize();
  }

  logout() {
    this.unsetSession();
    // navigate to the home route
    history.replace("/");
  }

  handleAuthentication = () => {
    this.auth0.parseHash((err, authResult) => {
      if (authResult && authResult.accessToken && authResult.idToken) {
        this.setSession(authResult);
        // store in db
        this.auth0.client.userInfo(authResult.accessToken, function(err, user) {
          // Now you have the user's information
          history.replace("/");
        });
      } else if (err) {
        history.replace("/");
        console.error(err);
        alert(`Error: ${err.error} - ${err.errorDescription}`);
      }
    });
  };

  setSession(authResult) {
    // Set the time that the access token will expire at
    let expiresAt = JSON.stringify(
      authResult.expiresIn * 1000 + new Date().getTime()
    );
    localStorage.setItem("auth0:access_token", authResult.accessToken);
    localStorage.setItem("auth0:id_token", authResult.idToken);
    localStorage.setItem("auth0:expires_at", expiresAt);
    localStorage.setItem("auth0:id_token:sub", authResult.idTokenPayload.sub);
  }

  unsetSession() {
    // Clear access token and ID token from local storage
    localStorage.removeItem("auth0:access_token");
    localStorage.removeItem("auth0:id_token");
    localStorage.removeItem("auth0:expires_at");
    localStorage.removeItem("auth0:id_token:sub");
  }

  isAuthenticated() {
    // Check whether the current time is past the
    // access token's expiry time
    let expiresAt = JSON.parse(localStorage.getItem("auth0:expires_at"));
    return new Date().getTime() < expiresAt;
  }
}
