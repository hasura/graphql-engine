import React from "react";
import { Route, Router } from "react-router-dom";

import Home from "./components/Home/Home";
import Callback from "./components/Callback/Callback";
import Auth from "./components/Auth/Auth";
import LandingPage from "./components/LandingPage/LandingPage";
import history from "./utils/history";

import { ApolloProvider } from "react-apollo";
import makeApolloClient from "./apollo";

const client = makeApolloClient();

const provideClient = component => {
  return <ApolloProvider client={client}>{component}</ApolloProvider>;
};

const auth = new Auth();

const handleAuthentication = ({ location }) => {
  if (/access_token|id_token|error/.test(location.hash)) {
    auth.handleAuthentication(client);
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
            provideClient(
              <LandingPage auth={auth} client={client} {...props} />
            )
          }
        />
        <Route
          exact
          path="/home"
          render={props =>
            provideClient(<Home auth={auth} client={client} {...props} />)
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
