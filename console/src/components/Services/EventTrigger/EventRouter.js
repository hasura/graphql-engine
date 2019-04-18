import React from 'react';
// import {push} fropm 'react-router-redux';
import { Route, IndexRedirect } from 'react-router';
import globals from '../../../Globals';

import {
  schemaConnector,
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
  loadProcessedEvents,
  loadPendingEvents,
  loadRunningEvents,
} from '../EventTrigger/EventActions';

import { SERVER_CONSOLE_MODE } from '../../../constants';

const makeEventRouter = (
  connect,
  store,
  composeOnEnterHooks,
  requireSchema,
  requireProcessedEvents,
  requirePendingEvents,
  requireRunningEvents,
  migrationRedirects
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
        <Route path="triggers" component={schemaConnector(connect)} />
        <Route
          path="triggers/:trigger/processed"
          component={processedEventsConnector(connect)}
          onEnter={composeOnEnterHooks([requireProcessedEvents])}
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
        onEnter={composeOnEnterHooks([migrationRedirects])}
        component={addTriggerConnector(connect)}
      />
      <Route
        path="manage/triggers/:trigger/modify"
        onEnter={composeOnEnterHooks([migrationRedirects])}
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

    Promise.all([store.dispatch(loadTriggers())]).then(
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

  const requireProcessedEvents = (nextState, replaceState, cb) => {
    const {
      triggers: { processedEvents },
    } = store.getState();

    if (processedEvents.length) {
      cb();
      return;
    }

    Promise.all([store.dispatch(loadProcessedEvents())]).then(
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

  const migrationRedirects = (nextState, replaceState, cb) => {
    const state = store.getState();
    if (!state.main.migrationMode) {
      replaceState(globals.urlPrefix + '/events/manage');
      cb();
    }
    cb();
  };

  const consoleModeRedirects = (nextState, replaceState, cb) => {
    if (globals.consoleMode === SERVER_CONSOLE_MODE) {
      replaceState(globals.urlPrefix + '/events/manage');
      cb();
    }
    cb();
  };

  return {
    makeEventRouter: makeEventRouter(
      connect,
      store,
      composeOnEnterHooks,
      requireSchema,
      requireProcessedEvents,
      requirePendingEvents,
      requireRunningEvents,
      migrationRedirects,
      consoleModeRedirects
    ),
    requireSchema,
    migrationRedirects,
  };
};

export default eventRouterUtils;
