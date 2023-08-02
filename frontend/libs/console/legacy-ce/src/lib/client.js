/**
 * THIS IS THE ENTRY POINT FOR THE CLIENT, JUST LIKE server.js IS THE ENTRY POINT FOR THE SERVER.
 */

import React from 'react';
import ReactDOM from 'react-dom';
import { useBasename } from 'history';
import { Provider } from 'react-redux';
import { Router, browserHistory } from 'react-router';
import { syncHistoryWithStore } from 'react-router-redux';

import { startTracing } from './features/Analytics';
import { ReactQueryProvider } from './lib/reactQuery';

import globals from './Globals';
import { store } from './store';
import getRoutes from './routes';
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

// Main routes and rendering
const Main = () => {
  const routeHistory = useBasename(() => history)({
    basename: globals.urlPrefix,
  });
  return (
    <ReactQueryProvider>
      <Router
        history={routeHistory}
        routes={getRoutes(store)}
        onUpdate={hashLinkScroll}
      />
    </ReactQueryProvider>
  );
};

export const App = () => (
  <Provider store={store} key="provider">
    <Main />
  </Provider>
);
