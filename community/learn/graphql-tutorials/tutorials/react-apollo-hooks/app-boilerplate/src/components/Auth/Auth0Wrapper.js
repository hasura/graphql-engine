import React, { useEffect, useState } from "react";
import auth0 from "auth0-js";

import history from "../../utils/history";
import { AUTH_CONFIG } from "./auth0-variables";
import App from "../App";
import Callback from "./Callback";
import Login from "./Login";

function Auth0Wrapper(props) {
  const auth0Val = new auth0.WebAuth({
    domain: AUTH_CONFIG.domain,
    clientID: AUTH_CONFIG.clientId,
    redirectUri: AUTH_CONFIG.callbackUrl,
    audience: `https://${AUTH_CONFIG.domain}/userinfo`,
    responseType: "token id_token",
    scope: "openid profile"
  });

  const [state, setState] = useState({
    isAuthenticated: false, // This can be true, false, 'loading'
    idToken: null
  });

  let accessToken, idToken, expiresAt;

  const login = () => {
    auth0Val.authorize();
  };

  const handleAuthentication = () => {
    setState({ isAuthenticated: "loading" });

    auth0Val.parseHash((err, authResult) => {
      if (authResult && authResult.accessToken && authResult.idToken) {
        setSession(authResult);
      } else if (err) {
        logout();
        console.error(err);
        alert(`Error: ${err.error} - ${err.errorDescription}`);
      }
    });
  };

  const getAccessToken = () => {
    return accessToken;
  };

  const getIdToken = () => {
    return idToken;
  };

  const setSession = authResult => {
    // Set isLoggedIn flag in localStorage
    localStorage.setItem("isLoggedIn", "true");

    // Set the time that the access token will expire at
    let expiresAtTime = authResult.expiresIn * 1000 + new Date().getTime();
    accessToken = authResult.accessToken;
    idToken = authResult.idToken;
    expiresAt = expiresAtTime;

    // navigate to the home route
    history.replace("/");
    setState({
      isAuthenticated: true,
      idToken: authResult.idToken
    });
  };

  const renewSession = () => {
    setState({ isAuthenticated: "loading" });

    auth0Val.checkSession({}, (err, authResult) => {
      if (authResult && authResult.accessToken && authResult.idToken) {
        setSession(authResult);
      } else if (err) {
        logout();
        console.log(err);
        alert(
          `Could not get a new token (${err.error}: ${err.error_description}).`
        );
      }
    });
  };

  const logout = () => {
    // Remove tokens and expiry time
    accessToken = null;
    idToken = null;
    expiresAt = 0;

    // Remove isLoggedIn flag from localStorage
    localStorage.removeItem("isLoggedIn");

    auth0Val.logout({
      return_to: window.location.origin
    });

    // navigate to the home route
    history.replace("/");
    setState({
      isAuthenticated: false,
      idToken: null
    });
  }

  const isExpired = () => {
    // Check whether the current time is past the
    // access token's expiry time
    let expiresAtTime = expiresAt;
    return new Date().getTime() > expiresAtTime;
  };

  useEffect(() => {
    const location = props.location;
    if (
      location &&
      location.pathname.startsWith("/callback") &&
      /access_token|id_token|error/.test(location.hash)
    ) {
      handleAuthentication();
      return;
    }

    // On first load, check if we are already logged in and get the idTokens and things
    if (localStorage.getItem("isLoggedIn") === "true") {
      renewSession();
      return;
    }
  }, []);

  if (state.isAuthenticated === "loading") {
    return <Callback {...props} />;
  }

  if (!state.isAuthenticated) {
    return <Login loginHandler={login} />;
  }

  return <App {...props} auth={{ ...state, login: login, logout: logout }} />;
}
export default Auth0Wrapper
