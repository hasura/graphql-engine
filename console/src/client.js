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
import globals from './Globals';

import {
  filterEventsBlockList,
  filterPayloadAllowList,
} from './analyticsFilter';

const analyticsUrl = globals.analyticsUrl;
let analyticsConnection;
try {
  analyticsConnection = new WebSocket(analyticsUrl);
} catch (error) {
  console.error(error);
}

const onError = error => {
  console.log('WebSocket Error for Events' + error);
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

function analyticsLogger() {
  return next => action => {
    // Call the next dispatch method in the middleware chain.
    const returnValue = next(action);
    // check if analytics tracking is enabled
    if (globals.isAnalyticsEnabled) {
      const projectName = globals.dataApiUrl;

      const projectVersion =
        globals.projectVersion !== undefined ? globals.projectVersion : null;
      const projectType =
        globals.projectType !== undefined ? globals.projectType : 'graphql';
      const hasuraId =
        globals.hasuraId !== undefined && globals.hasuraId !== ''
          ? globals.hasuraId
          : null;
      const email =
        globals.email !== undefined && globals.email !== ''
          ? globals.email
          : null;
      const uuid =
        globals.uuid !== undefined && globals.uuid !== '' ? globals.uuid : null;

      const reqBody = {
        project_name: projectName,
        project_version: projectVersion,
        project_type: projectType,
        hasura_id: hasuraId,
        email: email,
        uuid: uuid,
      };
      const actionType = action.type;

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
          // When the connection is open, send some data to the server
          if (isLocationType) {
            // capture page views
            const payload = action.payload;
            reqBody.url = payload.pathname;
            reqBody.payload = JSON.stringify(payload);
            analyticsConnection.send(
              JSON.stringify({ data: reqBody, topic: 'console-pageviews' })
            ); // Send the data
          } else {
            // capture events
            reqBody.event_type = actionType;
            // check for allowed list
            if (filterPayloadAllowList.includes(actionType)) {
              reqBody.event_data = action.data
                ? JSON.stringify(action.data)
                : null;
            } else {
              reqBody.event_data = null;
            }
            analyticsConnection.send(
              JSON.stringify({ data: reqBody, topic: 'console-events' })
            ); // Send the data
          }
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
  _finalCreateStore = compose(
    applyMiddleware(
      thunk,
      routerMiddleware(browserHistory),
      createLogger(),
      analyticsLogger
    ),
    require('redux-devtools').persistState(
      window.location.href.match(/[?&]debug_session=([^&]+)\b/)
    )
  )(createStore);
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
  module.hot.accept('./reducer', () => {
    store.replaceReducer(require('./reducer'));
  });
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
