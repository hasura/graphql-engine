import React from 'react';
import { Connect } from 'react-redux';
import { Route, IndexRedirect, EnterHook, RouterState } from 'react-router';
import rightContainerConnector from '../../Common/Layout/RightContainer/RightContainer';
import Container from './Container';
import { fetchTriggers } from './ServerIO';
import globals from '../../../Globals';
import { Dispatch, ReplaceRouterState, ReduxStore } from '../../../types';
import {
  getDataEventsLandingRoute,
  getScheduledEventsLandingRoute,
  getAddSTRoute,
  getSTModifyRoute,
  getSTProcessedEventsRoute,
  getSTPendingEventsRoute,
  getSTInvocationLogsRoute,
  getAddETRoute,
  getETModifyRoute,
  getETProcessedEventsRoute,
  getETPendingEventsRoute,
  getETInvocationLogsRoute,
  eventsPrefix,
  scheduledEventsPrefix,
  dataEventsPrefix,
  adhocEventsPrefix,
  getAddAdhocEventRoute,
  getAdhocEventsLogsRoute,
  getAdhocPendingEventsRoute,
  getAdhocProcessedEventsRoute,
  getAdhocEventsInfoRoute,
} from '../../Common/utils/routesUtils';
import {
  AddScheduledTrigger,
  ScheduledTriggerLogs,
  ScheduledTriggeModify,
  ScheduledTriggerLanding,
  STPendingEvents,
  STProcessedEvents,
} from './CronTriggers';
import {
  AddEventTrigger,
  ModifyEventTrigger,
  EventTriggerLanding,
  ETPendingEvents,
  ETProcessedEvents,
  ETInvocationLogs,
} from './EventTriggers';
import {
  AdhocEventPendingEvents,
  AdhocEventProcessedEvents,
  AddAdhocEvent,
  AdhocEventLogs,
  AdhocEventsInfo,
} from './AdhocEvents';

const triggersInit = (dispatch: Dispatch): EnterHook => {
  return (
    nextState: RouterState,
    replaceState: ReplaceRouterState,
    cb?: VoidFunction
  ) => {
    Promise.all([dispatch(fetchTriggers(null))]).then(
      () => {
        if (cb) {
          cb();
        }
      },
      () => {
        replaceState(globals.urlPrefix);
        if (cb) {
          cb();
        }
      }
    );
  };
};

const getTriggersRouter = (
  connect: Connect,
  store: ReduxStore,
  composeOnEnterHooks: (hooks: EnterHook[]) => EnterHook
) => {
  return (
    <Route
      path={eventsPrefix}
      component={Container(connect)}
      onEnter={composeOnEnterHooks([triggersInit(store.dispatch)])}
    >
      <IndexRedirect to={dataEventsPrefix} />
      <Route
        path={dataEventsPrefix}
        component={rightContainerConnector(connect)}
      >
        <IndexRedirect to={getDataEventsLandingRoute('relative')} />
        <Route path={getAddETRoute('relative')} component={AddEventTrigger} />
        <Route
          path={getETModifyRoute(':triggerName', 'relative')}
          component={ModifyEventTrigger}
        />
        <Route
          path={getETPendingEventsRoute(':triggerName', 'relative')}
          component={ETPendingEvents}
        />
        <Route
          path={getETProcessedEventsRoute(':triggerName', 'relative')}
          component={ETProcessedEvents}
        />
        <Route
          path={getETInvocationLogsRoute(':triggerName', 'relative')}
          component={ETInvocationLogs}
        />
        <Route
          path={getDataEventsLandingRoute('relative')}
          component={EventTriggerLanding}
        />
      </Route>
      <Route
        path={scheduledEventsPrefix}
        component={rightContainerConnector(connect)}
      >
        <IndexRedirect to={getScheduledEventsLandingRoute('relative')} />
        <Route
          path={getAddSTRoute('relative')}
          component={AddScheduledTrigger}
        />
        <Route
          path={getScheduledEventsLandingRoute('relative')}
          component={ScheduledTriggerLanding}
        />
        <Route
          path={getSTInvocationLogsRoute(':triggerName', 'relative')}
          component={ScheduledTriggerLogs}
        />
        <Route
          path={getSTPendingEventsRoute(':triggerName', 'relative')}
          component={STPendingEvents}
        />
        <Route
          path={getSTProcessedEventsRoute(':triggerName', 'relative')}
          component={STProcessedEvents}
        />
        <Route
          path={getSTModifyRoute(':triggerName', 'relative')}
          component={ScheduledTriggeModify}
        />
      </Route>
      <Route
        path={adhocEventsPrefix}
        component={rightContainerConnector(connect)}
      >
        <IndexRedirect to={getAdhocEventsInfoRoute('relative')} />
        <Route
          path={getAddAdhocEventRoute('relative')}
          component={AddAdhocEvent}
        />
        <Route
          path={getAdhocEventsLogsRoute('relative')}
          component={AdhocEventLogs}
        />
        <Route
          path={getAdhocPendingEventsRoute('relative')}
          component={AdhocEventPendingEvents}
        />
        <Route
          path={getAdhocProcessedEventsRoute('relative')}
          component={AdhocEventProcessedEvents}
        />
        <Route
          path={getAdhocEventsInfoRoute('relative')}
          component={AdhocEventsInfo}
        />
      </Route>
    </Route>
  );
};

export default getTriggersRouter;
