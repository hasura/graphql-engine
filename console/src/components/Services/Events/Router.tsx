import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import rightContainerConnector from '../../Common/Layout/RightContainer/RightContainer';
import Container from './Containers/Main';
import { fetchTriggers } from './ServerIO';
import globals from '../../../Globals';
import { ReduxState } from '../../../Types';
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
} from '../../Common/utils/routesUtils';
import {
  AddScheduledTrigger,
  ScheduledTriggerLogs,
  ScheduledTriggeModify,
  ScheduledTriggerLanding,
  STPendingEvents,
  STProcessedEvents,
} from './ScheduledTriggers';
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
} from './AdhocEvents';

const triggersInit = ({ dispatch }: { dispatch: any }) => {
  return (nextState: ReduxState, replaceState: any, cb: VoidFunction) => {
    Promise.all([dispatch(fetchTriggers(null))]).then(
      () => {
        cb();
      },
      () => {
        replaceState(globals.urlPrefix);
        cb();
      }
    );
  };
};

const getTriggersRouter = (
  connect: any,
  store: any,
  composeOnEnterHooks: any
) => {
  return (
    <Route
      path={eventsPrefix}
      component={Container(connect)}
      onEnter={composeOnEnterHooks([triggersInit(store)])}
    >
      <IndexRedirect to={dataEventsPrefix} />
      <Route
        path={dataEventsPrefix}
        component={rightContainerConnector(connect)}
      >
        <IndexRedirect to={getDataEventsLandingRoute('relative')} />
        <Route
          path={getAddETRoute('relative')}
          component={AddEventTrigger(connect)}
        />
        <Route
          path={getETModifyRoute(':triggerName', 'relative')}
          component={ModifyEventTrigger(connect)}
        />
        <Route
          path={getETPendingEventsRoute(':triggerName', 'relative')}
          component={ETPendingEvents(connect)}
        />
        <Route
          path={getETProcessedEventsRoute(':triggerName', 'relative')}
          component={ETProcessedEvents(connect)}
        />
        <Route
          path={getETInvocationLogsRoute(':triggerName', 'relative')}
          component={ETInvocationLogs(connect)}
        />
        <Route
          path={getDataEventsLandingRoute('relative')}
          component={EventTriggerLanding(connect)}
        />
      </Route>
      <Route
        path={scheduledEventsPrefix}
        component={rightContainerConnector(connect)}
      >
        <IndexRedirect to={getScheduledEventsLandingRoute('relative')} />
        <Route
          path={getAddSTRoute('relative')}
          component={AddScheduledTrigger(connect)}
        />
        <Route
          path={getScheduledEventsLandingRoute('relative')}
          component={ScheduledTriggerLanding(connect)}
        />
        <Route
          path={getSTInvocationLogsRoute(':triggerName', 'relative')}
          component={ScheduledTriggerLogs(connect)}
        />
        <Route
          path={getSTPendingEventsRoute(':triggerName', 'relative')}
          component={STPendingEvents(connect)}
        />
        <Route
          path={getSTProcessedEventsRoute(':triggerName', 'relative')}
          component={STProcessedEvents(connect)}
        />
        <Route
          path={getSTModifyRoute(':triggerName', 'relative')}
          component={ScheduledTriggeModify(connect)}
        />
      </Route>
      <Route
        path={adhocEventsPrefix}
        component={rightContainerConnector(connect)}
      >
        <IndexRedirect to={getAddAdhocEventRoute('relative')} />
        <Route
          path={getAddAdhocEventRoute('relative')}
          component={AddAdhocEvent(connect)}
        />
        <Route
          path={getAdhocEventsLogsRoute('relative')}
          component={AdhocEventLogs(connect)}
        />
        <Route
          path={getAdhocPendingEventsRoute('relative')}
          component={AdhocEventPendingEvents(connect)}
        />
        <Route
          path={getAdhocProcessedEventsRoute('relative')}
          component={AdhocEventProcessedEvents(connect)}
        />
      </Route>
    </Route>
  );
};

export default getTriggersRouter;
