/**
 * THIS IS THE ENTRY POINT FOR THE CLIENT, JUST LIKE server.js IS THE ENTRY POINT FOR THE SERVER.
 */
import React from 'react';
import { Provider } from 'react-redux';
import { ReactQueryProvider, startTracing } from '@hasura/console-legacy-ce';

import { Router, browserHistory } from 'react-router';
import { syncHistoryWithStore } from 'react-router-redux';
import { useBasename } from 'history';

import getRoutes from './routes';
import store from './store';

import globals from './Globals';

startTracing(globals, window.__env);

const hashLinkScroll = () => {
  const { hash } = window.location;
  if (hash !== '') {
    // Push onto callback queue so it runs after the DOM is updated,
    // this is required when navigating from a different page so that
    // the element is rendered on the page before trying to getElementById.
    setTimeout(() => {
      const id = hash.replace('#', '');
      const element = document.getElementById(id);
      if (element) {
        element.scrollIntoView();
      }
    }, 0);
  } else {
    // This is a hack to solve the issue with scroll retention during page change.
    setTimeout(() => {
      const element = document.getElementsByTagName('body');
      if (element && element.length > 0) {
        element[0].scrollIntoView();
      }
    }, 0);
  }
};

const history = syncHistoryWithStore(browserHistory, store);

/* ****************************************************************** */

// Enable hot reloading
if (__DEVELOPMENT__ && module.hot) {
  /*
  module.hot.accept('./reducer', () => {
    store.replaceReducer(require('./reducer'));
  });
  */
  module.hot.accept();
}

// Main routes and rendering
const main = (
  <ReactQueryProvider>
    <Router
      history={useBasename(() => history)({ basename: globals.urlPrefix })}
      routes={getRoutes(store)}
      onUpdate={hashLinkScroll}
    />
  </ReactQueryProvider>
);

export const Main = () => (
  <Provider store={store} key="provider">
    {main}
  </Provider>
);

if (process.env.NODE_ENV !== 'production') {
  window.React = React; // enable debugger
}
