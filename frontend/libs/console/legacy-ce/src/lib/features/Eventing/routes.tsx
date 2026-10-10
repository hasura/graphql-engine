import { Route, Navigate } from 'react-router';
import Container from './components/Container';
import { dataRoutes } from '@hasura/shared/utils';
import { RightContainerRoute } from '../../components/Common/Layout/RightContainer';
import AddScheduledTrigger from './CronTriggers/components/Add';
import ScheduledTriggerLanding from './CronTriggers/components/Landing';
import ScheduledTriggerLogs from './CronTriggers/components/Logs';
import ModifyScheduledTrigger from './CronTriggers/components/Modify';
import STProcessedEvents from './CronTriggers/components/ProcessedEvents';
import STPendingEvents from './CronTriggers/components/PendingEvents';
import AdhocEventPendingEvents from './AdhocEvents/components/PendingEvents';
import AddAdhocEvent from './AdhocEvents/components/Add';
import AdhocEventLogs from './AdhocEvents/components/Logs';
import AdhocEventProcessedEvents from './AdhocEvents/components/ProcessedEvents';
import AdhocEventsInfo from './AdhocEvents/components/Info';
import TriggerContainer from './EventTriggers/components/TriggerContainer';
import ETInvocationLogs from './EventTriggers/components/ETInvocationLogs';
import AddEventTrigger from './EventTriggers/components/Add';
import EventTriggerLanding from './EventTriggers/components/Landing';
import ModifyEventTrigger from './EventTriggers/components/Modify';
import ETPendingEvents from './EventTriggers/components/PendingEvents';
import ETProcessedEvents from './EventTriggers/components/ProcessedEvents';

const getEventRoutes = () => (
  <Route path={dataRoutes.eventsPrefix} element={<Container />}>
    <Route
      index
      element={<Navigate to={dataRoutes.dataEventsPrefix} replace />}
    />
    <Route path={dataRoutes.dataEventsPrefix} element={<RightContainerRoute />}>
      <Route
        index
        element={
          <Navigate
            to={dataRoutes.getDataEventsLandingRoute('relative')}
            replace
          />
        }
      />
      <Route
        path={dataRoutes.getAddETRoute('relative')}
        element={<AddEventTrigger />}
      />
      <Route
        path={dataRoutes.getDataEventsLandingRoute('relative')}
        element={<EventTriggerLanding />}
      />
      <Route path=":triggerName" element={<TriggerContainer />}>
        <Route
          path={dataRoutes.getETModifyRoute({ type: 'relative' })}
          element={<ModifyEventTrigger />}
        />
        <Route
          path={dataRoutes.getETPendingEventsRoute('relative')}
          element={<ETPendingEvents />}
        />
        <Route
          path={dataRoutes.getETProcessedEventsRoute('relative')}
          element={<ETProcessedEvents />}
        />
        <Route
          path={dataRoutes.getETInvocationLogsRoute('relative')}
          element={<ETInvocationLogs />}
        />
      </Route>
    </Route>
    <Route
      path={dataRoutes.scheduledEventsPrefix}
      element={<RightContainerRoute />}
    >
      <Route
        index
        element={
          <Navigate
            to={dataRoutes.getScheduledEventsLandingRoute('relative')}
            replace
          />
        }
      />
      <Route
        path={dataRoutes.getAddSTRoute('relative')}
        element={<AddScheduledTrigger />}
      />
      <Route
        path={dataRoutes.getScheduledEventsLandingRoute('relative')}
        element={<ScheduledTriggerLanding />}
      />
      <Route
        path={dataRoutes.getSTInvocationLogsRoute(':triggerName', 'relative')}
        element={<ScheduledTriggerLogs />}
      />
      <Route
        path={dataRoutes.getSTPendingEventsRoute(':triggerName', 'relative')}
        element={<STPendingEvents />}
      />
      <Route
        path={dataRoutes.getSTProcessedEventsRoute(':triggerName', 'relative')}
        element={<STProcessedEvents />}
      />
      <Route
        path={dataRoutes.getSTModifyRoute(':triggerName', 'relative')}
        element={<ModifyScheduledTrigger />}
      />
    </Route>
    <Route
      path={dataRoutes.adhocEventsPrefix}
      element={<RightContainerRoute />}
    >
      <Route
        index
        element={
          <Navigate
            to={dataRoutes.getAdhocEventsInfoRoute('relative')}
            replace
          />
        }
      />
      <Route
        path={dataRoutes.getAddAdhocEventRoute('relative')}
        element={<AddAdhocEvent />}
      />
      <Route
        path={dataRoutes.getAdhocEventsLogsRoute('relative')}
        element={<AdhocEventLogs />}
      />
      <Route
        path={dataRoutes.getAdhocPendingEventsRoute('relative')}
        element={<AdhocEventPendingEvents />}
      />
      <Route
        path={dataRoutes.getAdhocProcessedEventsRoute('relative')}
        element={<AdhocEventProcessedEvents />}
      />
      <Route
        path={dataRoutes.getAdhocEventsInfoRoute('relative')}
        element={<AdhocEventsInfo />}
      />
    </Route>
  </Route>
);

export default getEventRoutes;
