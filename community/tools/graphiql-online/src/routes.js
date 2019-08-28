import React from 'react';
import { Route, IndexRoute, IndexRedirect } from 'react-router';

import { connect } from 'react-redux';

import { App, Main, PageNotFound } from 'components';

import generatedApiExplorer from './components/Services/ApiExplorer/ApiExplorerGenerator';

import generatedVoyagerConnector from './components/Services/VoyagerView/VoyagerView';

import generatedLoginConnector from './components/Login/Login';


const routes = store => {
  // loads schema
  return (
    <Route path="/" component={App}>
      <Route path="">
        <IndexRoute component={generatedLoginConnector(connect)} />
      </Route>
      <Route path="/graphiql" component={generatedApiExplorer(connect)} />
      <Route
        path="voyager-view"
        component={generatedVoyagerConnector(connect)}
      />
      <Route path="404" component={PageNotFound} status="404" />
      <Route path="*" component={PageNotFound} status="404" />
    </Route>
  );
};

export default routes;
