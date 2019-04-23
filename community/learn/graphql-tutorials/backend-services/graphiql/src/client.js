/**
 * THIS IS THE ENTRY POINT FOR THE CLIENT, JUST LIKE server.js IS THE ENTRY POINT FOR THE SERVER.
 */
// import 'babel-polyfill';

import React from 'react';
import ReactDOM from 'react-dom';
import { createLogger } from 'redux-logger';
import thunk from 'redux-thunk';

import { Provider } from 'react-redux';

import { Router, browserHistory } from 'react-router';
import { routerMiddleware, syncHistoryWithStore } from 'react-router-redux';
import { compose, createStore, applyMiddleware } from 'redux';
import { useBasename } from 'history';

import getRoutes from './routes';

import reducer from './reducer';

// Create the store
let _finalCreateStore;

if (__DEVELOPMENT__) {
  _finalCreateStore = compose(
    applyMiddleware(thunk, routerMiddleware(browserHistory), createLogger()),
    require('redux-devtools').persistState(
      window.location.href.match(/[?&]debug_session=([^&]+)\b/)
    )
  )(createStore);
} else {
  _finalCreateStore = compose(
    applyMiddleware(thunk, routerMiddleware(browserHistory))
  )(createStore);
}

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

const store = _finalCreateStore(reducer);
const history = syncHistoryWithStore(browserHistory, store);

/* ****************************************************************** */

// Enable hot reloading
if (__DEVELOPMENT__ && module.hot) {
  module.hot.accept('./reducer', () => {
    store.replaceReducer(require('./reducer'));
  });
}

// Main routes and rendering
const main = (
  <Router
    history={useBasename(() => history)({ basename: '/graphql/graphiql' })}
    // history={useBasename(() => history)({ basename: '/' })}
    routes={getRoutes(store)}
    onUpdate={hashLinkScroll}
  />
);

const dest = document.getElementById('content');
ReactDOM.render(
  <Provider store={store} key="provider">
    {main}
  </Provider>,
  dest
);

if (process.env.NODE_ENV !== 'production') {
  window.React = React; // enable debugger
}
