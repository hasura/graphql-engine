import React from 'react';
import { Route, IndexRedirect } from 'react-router';

import Container from './Container';
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
import { RightContainer } from '../../Common/Layout/RightContainer';
import { TriggerContainerConnector } from './EventTriggers/TriggerContainer';

const triggerRoutes = (
  <Route path={eventsPrefix} component={Container}>
    <IndexRedirect to={dataEventsPrefix} />
    <Route path={dataEventsPrefix} component={RightContainer}>
      <IndexRedirect to={getDataEventsLandingRoute('relative')} />
      <Route path={getAddETRoute('relative')} component={AddEventTrigger} />
      <Route
        path={getDataEventsLandingRoute('relative')}
        component={EventTriggerLanding}
      />
      <Route path=":triggerName" component={TriggerContainerConnector}>
        <Route
          path={getETModifyRoute({ type: 'relative' })}
          component={ModifyEventTrigger}
        />
        <Route
          path={getETPendingEventsRoute('relative')}
          component={ETPendingEvents}
        />
        <Route
          path={getETProcessedEventsRoute('relative')}
          component={ETProcessedEvents}
        />
        <Route
          path={getETInvocationLogsRoute('relative')}
          component={ETInvocationLogs}
        />
      </Route>
    </Route>
    <Route path={scheduledEventsPrefix} component={RightContainer}>
      <IndexRedirect to={getScheduledEventsLandingRoute('relative')} />
      <Route path={getAddSTRoute('relative')} component={AddScheduledTrigger} />
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
    <Route path={adhocEventsPrefix} component={RightContainer}>
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

export default triggerRoutes;
