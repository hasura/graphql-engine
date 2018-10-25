import React from 'react';
import { Route, IndexRoute } from 'react-router';
import { connect } from 'react-redux';
import { App, PageNotFound } from 'components';
import generatedApiExplorer from './components/ApiExplorer/ApiExplorerGenerator';
import generatedLoginComponent from './components/Login/LoginGenerator';

const routes = () => {
  // loads schema
  return (
    <Route path="/" component={App}>
      <Route path="">
        <IndexRoute component={generatedLoginComponent(connect)} />
      </Route>
      <Route path="/graphiql" component={generatedApiExplorer(connect)} />
      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
