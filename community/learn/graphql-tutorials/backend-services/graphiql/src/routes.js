import React from 'react';
import { Route, IndexRoute } from 'react-router';
import { connect } from 'react-redux';
import { App, PageNotFound } from 'components';
import generatedApiExplorer from './components/ApiExplorer/ApiExplorerGenerator';
import Auth from './components/Auth/Auth';
import Callback from './components/Callback/Callback';

const auth = new Auth();

const routes = () => {
  // loads schema
  return (
    <Route path="/" component={App} auth={auth}>
      {/*
      <Route path="">
        <IndexRoute component={generatedLoginComponent(connect)} />
      </Route>
      */}
      <Route path="">
        <IndexRoute auth={auth} component={generatedApiExplorer(connect)} />
      </Route>
      <Route path="404" component={PageNotFound} status="404" />
      <Route exact path="/callback" auth={auth} component={Callback} />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
