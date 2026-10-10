import React from 'react';
import { Outlet, useLocation, useParams } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import {
  ADHOC_EVENTS_HEADING,
  DATA_EVENTS_HEADING,
  CRON_EVENTS_HEADING,
} from '@hasura/shared/types';
import { dataRoutes } from '@hasura/shared/utils';
import CronTriggerSidebar from '../../CronTriggers/components/CronTriggerSidebar';
import EventSidebar from '../../EventTriggers/components/EventSidebar';
import { LeftContainer, LeftSidebar, PageContainer } from '@hasura/shared/ui';

const Container: React.FC = () => {
  const { pathname: currentLocation } = useLocation();
  const { triggerName } = useParams();
  const isEventRoute = dataRoutes.isDataEventsRoute(currentLocation);
  const isCronRoute = dataRoutes.isScheduledEventsRoute(currentLocation);
  const isOneOffRoute = dataRoutes.isAdhocScheduledEventRoute(currentLocation);

  const sidebarContent = (
    <Analytics name="EventsSidebar" {...REDACT_EVERYTHING}>
      <LeftSidebar
        items={[
          {
            isActive: isEventRoute,
            label: DATA_EVENTS_HEADING,
            to: dataRoutes.getDataEventsLandingRoute(),
            children: isEventRoute ? (
              <EventSidebar triggerName={triggerName} />
            ) : null,
          },
          {
            isActive: isCronRoute,
            label: CRON_EVENTS_HEADING,
            to: dataRoutes.getScheduledEventsLandingRoute(),
            children: dataRoutes.isScheduledEventsRoute(currentLocation) ? (
              <CronTriggerSidebar triggerName={triggerName} />
            ) : null,
          },
          {
            isActive: isOneOffRoute,
            label: ADHOC_EVENTS_HEADING,
            to: dataRoutes.getAdhocEventsRoute('absolute', ''),
          },
        ]}
      />
    </Analytics>
  );

  const helmetTitle = 'Triggers | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmetTitle} leftContainer={leftContainer}>
      <Outlet />
    </PageContainer>
  );
};

export default Container;
