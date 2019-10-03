import React from 'react';
// import {push} fropm 'react-router-redux';
import { Route, IndexRedirect } from 'react-router';
import globals from '../../../Globals';

import {
  landingConnector,
  addTriggerConnector,
  modifyTriggerConnector,
  processedEventsConnector,
  pendingEventsConnector,
  runningEventsConnector,
  eventPageConnector,
  streamingLogsConnector,
} from '.';

import { rightContainerConnector } from '../../Common/Layout';

import {
  loadTriggers,
  loadPendingEvents,
  loadRunningEvents,
} from '../EventTrigger/EventActions';

const makeEventRouter = (
  connect,
  store,
  composeOnEnterHooks,
  requireSchema,
  requirePendingEvents,
  requireRunningEvents
) => {
  return (
    <Route
      path="events"
      component={eventPageConnector(connect)}
      onEnter={composeOnEnterHooks([requireSchema])}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={rightContainerConnector(connect)}>
        <IndexRedirect to="triggers" />
        <Route path="triggers" component={landingConnector(connect)} />
        <Route
          path="triggers/:trigger/processed"
          component={processedEventsConnector(connect)}
        />
        <Route
          path="triggers/:trigger/pending"
          component={pendingEventsConnector(connect)}
          onEnter={composeOnEnterHooks([requirePendingEvents])}
        />
        <Route
          path="triggers/:trigger/running"
          component={runningEventsConnector(connect)}
          onEnter={composeOnEnterHooks([requireRunningEvents])}
        />
        <Route
          path="triggers/:trigger/logs"
          component={streamingLogsConnector(connect)}
        />
      </Route>
      <Route
        path="manage/triggers/add"
        component={addTriggerConnector(connect)}
      />
      <Route
        path="manage/triggers/:trigger/modify"
        component={modifyTriggerConnector(connect)}
      />
    </Route>
  );
};

const eventRouterUtils = (connect, store, composeOnEnterHooks) => {
  const requireSchema = (nextState, replaceState, cb) => {
    // check if admin secret is available in localstorage. if so use that.
    // if localstorage admin secret didn't work, redirect to login (meaning value has changed)
    // if admin secret is not available in localstorage, check if cli is giving it via window.__env
    // if admin secret is not available in localstorage and cli, make a api call to data without admin secret.
    // if the api fails, then redirect to login - this is a fresh user/browser flow
    const {
      triggers: { triggerList },
    } = store.getState();

    if (triggerList.length) {
      cb();
      return;
    }

    Promise.all([store.dispatch(loadTriggers([]))]).then(
      () => {
        cb();
      },
      () => {
        // alert('Could not load schema.');
        replaceState(globals.urlPrefix);
        cb();
      }
    );
  };

  const requirePendingEvents = (nextState, replaceState, cb) => {
    const {
      triggers: { pendingEvents },
    } = store.getState();

    if (pendingEvents.length) {
      cb();
      return;
    }

    Promise.all([store.dispatch(loadPendingEvents())]).then(
      () => {
        cb();
      },
      () => {
        // alert('Could not load schema.');
        replaceState(globals.urlPrefix);
        cb();
      }
    );
  };

  const requireRunningEvents = (nextState, replaceState, cb) => {
    const {
      triggers: { runningEvents },
    } = store.getState();

    if (runningEvents.length) {
      cb();
      return;
    }

    Promise.all([store.dispatch(loadRunningEvents())]).then(
      () => {
        cb();
      },
      () => {
        // alert('Could not load schema.');
        replaceState(globals.urlPrefix);
        cb();
      }
    );
  };

  return {
    makeEventRouter: makeEventRouter(
      connect,
      store,
      composeOnEnterHooks,
      requireSchema,
      requirePendingEvents,
      requireRunningEvents
    ),
    requireSchema,
  };
};

export default eventRouterUtils;
