import React from 'react';
import { Link } from 'react-router';

import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from '../Sidebar/Sidebar';
import styles from '../../../Common/TableCommon/Table.scss';
import { Triggers } from '../Types';
import {
  getScheduledEventsLandingRoute,
  getDataEventsLandingRoute,
  isScheduledEventsRoute,
  isDataEventsRoute,
} from '../../../Common/utils/routesUtils';
import { findEventTrigger, findScheduledTrigger } from '../utils';

import { ReduxState } from '../../../../Types';

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
          Event Triggers
        </Link>
        <LeftSidebar
          triggers={triggers.event}
          service="event triggers"
          currentTrigger={currentEventTrigger}
        />
      </li>
      <li
        role="presentation"
        className={isScheduledEventsRoute(currentLocation) ? styles.active : ''}
      >
        <Link
          className={styles.linkBorder}
          to={getScheduledEventsLandingRoute()}
        >
          Scheduled Triggers
        </Link>
        <LeftSidebar
          triggers={triggers.scheduled}
          service="scheduled triggers"
          currentTrigger={currentScheduledTrigger}
        />
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

const mapStateToProps = (state: ReduxState) => {
  return {
    ...state.events,
  };
};

export default (connect: any) => connect(mapStateToProps)(Container);
