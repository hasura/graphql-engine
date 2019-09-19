import * as ReactDOM from 'react-dom';
import * as React from 'react';
import { Route, Router } from "react-router-dom";

import './styles/App.css';

import { Auth0Provider } from "./components/Auth/react-auth0-spa";
import history from "./utils/history";
import { AUTH_CONFIG } from './components/Auth/auth0-variables';

const onRedirectCallback = (appState: any) => {
  history.push(
    appState && appState.targetUrl
      ? appState.targetUrl
      : window.location.pathname
  );
};

const mainRoutes = (
  <Router history={history}>
    <Route path="/" render={props => (
		<Auth0Provider
		  domain={AUTH_CONFIG.domain}
		  client_id={AUTH_CONFIG.clientId}
		  redirect_uri={AUTH_CONFIG.callbackUrl}
		  onRedirectCallback={onRedirectCallback} />
	)} />
  </Router>
);

ReactDOM.render(mainRoutes, document.getElementById("root"));
