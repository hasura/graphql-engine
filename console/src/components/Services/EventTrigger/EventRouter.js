import React from 'react';
// import {push} fropm 'react-router-redux';
import { Route, IndexRedirect } from 'react-router';
import globals from '../../../Globals';

import { ConnectedRightContainer } from '../../Common/Layout';

import {
  loadTriggers,
  loadPendingEvents,
  loadRunningEvents,
} from '../EventTrigger/EventActions';
import ConnectedEventPageContainer from './EventPageContainer';
import ConnectedEventTriggerLanding from './Landing/EventTrigger';
import ConnectedProcessedEventsViewTable from './ProcessedEvents/ViewTable';
import ConnectedPendingEventsViewTable from './PendingEvents/ViewTable';
import ConnectedRunningEventsViewTable from './RunningEvents/ViewTable';
import ConnectedStreamingLogs from './StreamingLogs/Logs';
import ConnectedAddTrigger from './Add/AddTrigger';
import ConnectedModifyTrigger from './Modify/Connector';

const makeEventRouter = ({
  composeOnEnterHooks,
  requireSchema,
  requirePendingEvents,
  requireRunningEvents,
}) => {
  return (
    <Route
      path="events"
      component={ConnectedEventPageContainer}
      onEnter={composeOnEnterHooks([requireSchema])}
    >
      <IndexRedirect to="manage" />
      <Route path="manage" component={ConnectedRightContainer}>
        <IndexRedirect to="triggers" />
        <Route path="triggers" component={ConnectedEventTriggerLanding} />
        <Route
          path="triggers/:trigger/processed"
          component={ConnectedProcessedEventsViewTable}
        />
        <Route
          path="triggers/:trigger/pending"
          component={ConnectedPendingEventsViewTable}
          onEnter={composeOnEnterHooks([requirePendingEvents])}
        />
        <Route
          path="triggers/:trigger/running"
          component={ConnectedRunningEventsViewTable}
          onEnter={composeOnEnterHooks([requireRunningEvents])}
        />
        <Route
          path="triggers/:trigger/logs"
          component={ConnectedStreamingLogs}
        />
      </Route>
      <Route path="manage/triggers/add" component={ConnectedAddTrigger} />
      <Route
        path="manage/triggers/:trigger/modify"
        component={ConnectedModifyTrigger}
      />
    </Route>
  );
};

const eventRouterUtils = (store, composeOnEnterHooks) => {
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
    makeEventRouter: makeEventRouter({
      composeOnEnterHooks,
      requireSchema,
      requirePendingEvents,
      requireRunningEvents,
    }),
    requireSchema,
  };
};

export default eventRouterUtils;
