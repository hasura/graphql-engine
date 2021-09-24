import React from 'react';
import { Link, RouteComponentProps } from 'react-router';
import { connect, ConnectedProps } from 'react-redux';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from './Sidebar';
import styles from '../../Common/TableCommon/Table.scss';
import {
  ADHOC_EVENTS_HEADING,
  DATA_EVENTS_HEADING,
  CRON_EVENTS_HEADING,
} from './constants';
import {
  getScheduledEventsLandingRoute,
  getDataEventsLandingRoute,
  isScheduledEventsRoute,
  isDataEventsRoute,
  isAdhocScheduledEventRoute,
  getAdhocEventsRoute,
} from '../../Common/utils/routesUtils';
import { findEventTrigger, findScheduledTrigger } from './utils';

import { ReduxState } from '../../../types';

import { mapDispatchToPropsEmpty } from '../../Common/utils/reactUtils';
import { getEventTriggers, getCronTriggers } from '../../../metadata/selector';

interface Props extends InjectedProps {}

const Container: React.FC<Props> = props => {
  const {
    children,
    pathname: currentLocation,
    triggerName: currentTriggerName,
    eventTriggers,
    cronTriggers,
  } = props;

  let currentEventTrigger;
  let currentScheduledTrigger;

  if (currentTriggerName) {
    if (isDataEventsRoute(currentLocation)) {
      currentEventTrigger = findEventTrigger(currentTriggerName, eventTriggers);
    } else {
      currentScheduledTrigger = findScheduledTrigger(
        currentTriggerName,
        cronTriggers
      );
    }
  }

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={isDataEventsRoute(currentLocation) ? styles.active : ''}
      >
        {/* <li role="presentation" className={styles.active}>
          <Link
            className={styles.linkBorder}
            style={{
              paddingRight: '20px',
            }}
          >

          </Link>
        </li> */}
        <Link className={styles.linkBorder} to={getDataEventsLandingRoute()}>
          {DATA_EVENTS_HEADING}
        </Link>

        {isDataEventsRoute(currentLocation) ? (
          <LeftSidebar
            triggers={eventTriggers}
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
            triggers={cronTriggers}
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
          data-test="one-off-trigger"
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

type ExternalProps = RouteComponentProps<
  {
    triggerName: string;
  },
  unknown
>;

const mapStateToProps = (state: ReduxState, ownProps: ExternalProps) => {
  return {
    ...state.events,
    eventTriggers: getEventTriggers(state),
    cronTriggers: getCronTriggers(state),
    pathname: ownProps.location.pathname,
    triggerName: ownProps.params.triggerName,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const ContainerConnector = connector(Container);

export default ContainerConnector;
