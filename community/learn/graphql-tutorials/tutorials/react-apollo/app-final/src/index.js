import ReactDOM from "react-dom";
import React from "react";
import { Route, Router } from "react-router-dom";

import './styles/App.css';

import Auth0Wrapper from "./components/Auth/Auth0Wrapper";
import history from "./utils/history";

const mainRoutes = (
  <Router history={history}>
    <Route path="/" render={props => (<Auth0Wrapper {...props} />)} />
  </Router>
);

ReactDOM.render(mainRoutes, document.getElementById("root"));
