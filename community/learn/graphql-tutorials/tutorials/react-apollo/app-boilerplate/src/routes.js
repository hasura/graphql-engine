import React from "react";
import { Route, Router } from "react-router-dom";
import App from "./components/App";
import Callback from "./components/Callback/Callback";
import Auth from "./components/Auth/Auth";
import history from "./utils/history";

const auth = new Auth();

const handleAuthentication = ({ location }) => {
  if (/access_token|id_token|error/.test(location.hash)) {
    auth.handleAuthentication();
  }
};

export const makeMainRoutes = () => {
  return (
    <Router history={history}>
      <div>
        <Route
          exact
          path="/"
          render={props =>
            <App auth={auth} {...props} />
          }
        />
        <Route
          exact
          path="/callback"
          render={props => {
            handleAuthentication(props);
            return <Callback {...props} />;
          }}
        />
      </div>
    </Router>
  );
};
