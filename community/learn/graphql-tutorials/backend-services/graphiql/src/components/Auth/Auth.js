import auth0 from "auth0-js";
import { AUTH_CONFIG } from "./auth0-variables";
import { isReactNative } from '../ApiExplorer/utils';

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

  handleAuthentication = client => {
    this.auth0.parseHash((err, authResult) => {
      if (authResult && authResult.accessToken && authResult.idToken) {
        this.setSession(authResult);
      } else if (err) {
        console.error(err);
        window.location.replace("/graphql/graphiql");
        alert(`Error: ${err.error}. Check the console for further details.`);
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
    // navigate to the home route
    window.location.replace("/graphql/graphiql");
  }

  logout() {
    if (isReactNative()) {
      window.localStorage.removeItem('@learn.hasura.io:graphiql-react-native-token');
      window.localStorage.removeItem('@learn.hasura.io:graphiql-react-native-exp');
      window.location.replace("/graphql/graphiql?tutorial=react-native");
      return;
    }
    // Clear access token and ID token from local storage
    localStorage.removeItem("auth0:access_token");
    localStorage.removeItem("auth0:id_token");
    localStorage.removeItem("auth0:expires_at");
    localStorage.removeItem("auth0:id_token:sub");
    // navigate to the home route
    // history.replace("/home");
    window.location.replace("/graphql/graphiql");
  }

  isAuthenticated() {
    // Check whether the current time is past the
    // access token's expiry time
    if (!isReactNative()) {
      let expiresAt = JSON.parse(localStorage.getItem("auth0:expires_at"));
      return new Date().getTime() < expiresAt;
    } else {
      const token = window.localStorage.getItem('@learn.hasura.io:graphiql-react-native-token');
      const exp = window.localStorage.getItem('@learn.hasura.io:graphiql-react-native-exp');
      if (!exp || !token) {
        return false;
      }
      var currentTime = Math.floor(new Date().getTime() / 1000);
      return currentTime < exp;
    }
    return false;
  }
}
