import React from 'react';
import { Link } from 'react-router';

import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from '../Sidebar/Sidebar';
import styles from '../../../Common/TableCommon/Table.scss';
import { Triggers } from '../types';
import {
  ADHOC_EVENTS_HEADING,
  DATA_EVENTS_HEADING,
  CRON_EVENTS_HEADING,
} from '../constants';
import {
  getScheduledEventsLandingRoute,
  getDataEventsLandingRoute,
  isScheduledEventsRoute,
  isDataEventsRoute,
  isAdhocScheduledEventRoute,
  getAdhocEventsRoute,
} from '../../../Common/utils/routesUtils';
import { findEventTrigger, findScheduledTrigger } from '../utils';

import { MapReduxToProps, ComponentReduxConnector } from '../../../../types';

interface TriggersContainerProps extends React.ComponentProps<'div'> {
  triggers: Triggers;
  location: {
    pathname: string;
    triggerName?: string;
  };
}

const Container: React.FC<TriggersContainerProps> = props => {
  const { triggers, children, location } = props;

  const {
    pathname: currentLocation,
    triggerName: currentTriggerName,
  } = location;

  let currentEventTrigger;
  let currentScheduledTrigger;

  if (currentTriggerName) {
    if (isDataEventsRoute(currentLocation)) {
      currentEventTrigger = findEventTrigger(
        currentTriggerName,
        triggers.event
      );
    } else {
      currentScheduledTrigger = findScheduledTrigger(
        currentTriggerName,
        triggers.scheduled
      );
    }
  }

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={isDataEventsRoute(currentLocation) ? styles.active : ''}
      >
        <Link className={styles.linkBorder} to={getDataEventsLandingRoute()}>
          {DATA_EVENTS_HEADING}
        </Link>
        {isDataEventsRoute(currentLocation) ? (
          <LeftSidebar
            triggers={triggers.event}
            service="data"
            currentTrigger={currentEventTrigger}
          />
        ) : null}
      </li>
      <li
        role="presentation"
        className={isScheduledEventsRoute(currentLocation) ? styles.active : ''}
      >
        <Link
          className={styles.linkBorder}
          to={getScheduledEventsLandingRoute()}
        >
          {CRON_EVENTS_HEADING}
        </Link>
        {isScheduledEventsRoute(currentLocation) ? (
          <LeftSidebar
            triggers={triggers.scheduled}
            service="cron"
            currentTrigger={currentScheduledTrigger}
          />
        ) : null}
      </li>
      <li
        role="presentation"
        className={
          isAdhocScheduledEventRoute(currentLocation) ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={getAdhocEventsRoute('absolute', '')}
        >
          {ADHOC_EVENTS_HEADING}
        </Link>
      </li>
    </ul>
  );

  const helmetTitle = 'Triggers | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmetTitle} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

const mapStateToProps: MapReduxToProps = state => {
  return {
    ...state.events,
  };
};

const connector: ComponentReduxConnector = (connect: any) =>
  connect(mapStateToProps)(Container);

export default connector;
