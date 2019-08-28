/**
 * THIS IS THE ENTRY POINT FOR THE CLIENT, JUST LIKE server.js IS THE ENTRY POINT FOR THE SERVER.
 */
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
import globals from './Globals';
import Endpoints from './Endpoints';
import { filterEventsBlockList, sanitiseUrl } from './telemetryFilter';

const analyticsUrl = Endpoints.telemetryServer;
let analyticsConnection;
const { consoleMode, enableTelemetry, cliUUID } = window.__env;
const telemetryEnabled =
  enableTelemetry !== undefined && enableTelemetry === true;
if (telemetryEnabled) {
  try {
    analyticsConnection = new WebSocket(analyticsUrl);
  } catch (error) {
    console.error(error);
  }
}

const onError = error => {
  console.error('WebSocket Error for Events' + error);
};

const onClose = () => {
  try {
    analyticsConnection = new WebSocket(analyticsUrl);
  } catch (error) {
    console.error(error);
  }
  analyticsConnection.onclose = onClose();
  analyticsConnection.onerror = onError();
};

function analyticsLogger({ getState }) {
  return next => action => {
    // Call the next dispatch method in the middleware chain.
    const returnValue = next(action);
    // check if analytics tracking is enabled
    if (telemetryEnabled) {
      const serverVersion = getState().main.serverVersion;
      const actionType = action.type;
      const url = sanitiseUrl(window.location.pathname);
      const reqBody = {
        server_version: serverVersion,
        event_type: actionType,
        url,
        console_mode: consoleMode,
        cli_uuid: cliUUID,
        server_uuid: getState().telemetry.hasura_uuid,
      };

      let isLocationType = false;
      if (actionType === '@@router/LOCATION_CHANGE') {
        isLocationType = true;
      }
      // filter events
      if (!filterEventsBlockList.includes(actionType)) {
        if (
          analyticsConnection &&
          analyticsConnection.readyState === analyticsConnection.OPEN
        ) {
          // When the connection is open, send data to the server
          if (isLocationType) {
            // capture page views
            const payload = action.payload;
            reqBody.url = sanitiseUrl(payload.pathname);
          }
          analyticsConnection.send(
            JSON.stringify({ data: reqBody, topic: globals.telemetryTopic })
          ); // Send the data
          // check for possible error events and store more data?
        } else {
          // retry websocket connection
          // analyticsConnection = new WebSocket(analyticsUrl);
        }
      }
    }
    // This will likely be the action itself, unless
    // a middleware further in chain changed it.
    return returnValue;
  };
}

// Create the store
let _finalCreateStore;

if (__DEVELOPMENT__) {
  const tools = [
    applyMiddleware(
      thunk,
      routerMiddleware(browserHistory),
      createLogger({ diff: true, duration: true }),
      analyticsLogger
    ),
    require('redux-devtools').persistState(
      window.location.href.match(/[?&]debug_session=([^&]+)\b/)
    ),
  ];
  if (typeof window === 'object' && window.__REDUX_DEVTOOLS_EXTENSION__) {
    tools.push(window.__REDUX_DEVTOOLS_EXTENSION__());
  }
  _finalCreateStore = compose(...tools)(createStore);
} else {
  _finalCreateStore = compose(
    applyMiddleware(thunk, routerMiddleware(browserHistory), analyticsLogger)
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
  /*
  module.hot.accept('./reducer', () => {
    store.replaceReducer(require('./reducer'));
  });
  */
  module.hot.accept();
}

// Main routes and rendering
const main = (
  <Router
    history={useBasename(() => history)({ basename: globals.urlPrefix })}
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
