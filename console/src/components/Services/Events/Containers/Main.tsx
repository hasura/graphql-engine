import React from 'react';
import { Link } from 'react-router';

import LeftContainer from '../../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from '../Sidebar/Sidebar';
import styles from '../../../Common/TableCommon/Table.scss';
import { Triggers } from '../Types';
import { appPrefix } from '../constants';
import { EventsState } from '../state';
import {
  getScheduledEventsLandingRoute,
  getDataEventsLandingRoute,
  isScheduledEventsRoute,
  isDataEventsRoute
} from '../../../Common/utils/routesUtils';

interface TriggersContainerProps extends React.ComponentProps<'div'> {
  triggers: Triggers;
  location: {
    pathname: string;
  };
}

const Container: React.FC<TriggersContainerProps> = props => {
  const { triggers, children, location } = props;

  const currentLocation = location.pathname;

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          isDataEventsRoute(currentLocation) ? styles.active : ''
        }
      >
        <Link className={styles.linkBorder} to={getDataEventsLandingRoute()}>
          Event Triggers
        </Link>
        <LeftSidebar
          triggers={triggers.event}
          currentTrigger={{ name: '' }}
          service={'event triggers'}
        />
      </li>
      <li
        role="presentation"
        className={
          isScheduledEventsRoute(currentLocation) ? styles.active : ''
        }
      >
        <Link
          className={styles.linkBorder}
          to={getScheduledEventsLandingRoute()}
        >
          Scheduled Triggers
        </Link>
        <LeftSidebar
          triggers={triggers.scheduled}
          currentTrigger={{ name: '' }}
          service={'scheduled triggers'}
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

const mapStateToProps = (state: { events: EventsState }) => {
  return {
    ...state.events,
  };
};

export default (connect: any) => connect(mapStateToProps)(Container);
