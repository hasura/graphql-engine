import React from "react";
import { Route, Router } from "react-router-dom";

import Home from "./components/Home/Home";
import Callback from "./components/Callback/Callback";
import auth from "./components/Auth/Auth";
import LandingPage from "./components/LandingPage/LandingPage";
import history from "./utils/history";

import { ApolloProvider } from "react-apollo";
import makeApolloClient from "./apollo";

let client;

/*
const provideClient = (Component,renderProps) => {
  if(auth.isAuthenticated()) {
    console.log('IM Authenticated');
    if(!client) {
      console.log('client doesnt exist');
      client = makeApolloClient();
    }
    return (
      <ApolloProvider client={client}>
        <Component {...renderProps} auth={auth} client={client} />
      </ApolloProvider>);
  } else {
    console.log('HERE');
    if (localStorage.getItem('isLoggedIn') === 'true') {
      // eslint-disable-next-line
      auth.renewSession();
      if(!client) {
        console.log('client doesnt exist');
        client = makeApolloClient();
      }
      return (
        <ApolloProvider client={client}>
          <Component {...renderProps} auth={auth} client={client} />
        </ApolloProvider>);
    } else {
      return (<Component auth={auth} {...renderProps} />);
    }
  }
};
*/

const provideClient = (Component, renderProps) => {
  if (localStorage.getItem("isLoggedIn") === "true") {
    if (!client) {
      client = makeApolloClient();
    }
    return (
      <ApolloProvider client={client}>
        <Component {...renderProps} auth={auth} client={client} />
      </ApolloProvider>
    );
  } else {
    // if not login page, redirect to login page
    if (renderProps.match.path !== "/") {
      window.location.href = "/";
    } else {
      return <Component auth={auth} {...renderProps} />;
    }
  }
};
// const auth = new Auth();

const handleAuthentication = aClient => ({ location }) => {
  if (/access_token|id_token|error/.test(location.hash)) {
    auth.handleAuthentication(aClient);
  }
};

export const makeMainRoutes = () => {
  return (
    <Router history={history}>
      <div>
        <Route
          exact
          path="/"
          render={props => provideClient(LandingPage, props)}
        />
        <Route
          exact
          path="/home"
          render={props => provideClient(Home, props)}
        />
        <Route
          path="/callback"
          render={props => {
            handleAuthentication(client)(props);
            return <Callback {...props} />;
          }}
        />
      </div>
    </Router>
  );
};
