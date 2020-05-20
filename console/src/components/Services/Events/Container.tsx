import React from 'react';
import { Link, RouteComponentProps } from 'react-router';
import { Connect } from 'react-redux'

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import LeftSidebar from './Sidebar';
import styles from '../../Common/TableCommon/Table.scss';
import { Triggers } from './types';
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

import {
  ConnectInjectedProps,
  ReduxState,
  MapStateToProps
} from '../../../types';

type PropsFromState = {
  triggers: Triggers,
  pathname: string,
  triggerName: string
};

interface Props extends ConnectInjectedProps,PropsFromState {}

const Container: React.FC<Props> = props => {
  const {
    triggers,
    children,
    pathname: currentLocation,
    triggerName: currentTriggerName,
  } = props;

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

type ExternalProps = RouteComponentProps<{
  triggerName: string
},{}>

const mapStateToProps: MapStateToProps<PropsFromState, ExternalProps> = (state, ownProps) => {
  return {
    ...state.events,
    pathname: ownProps.location.pathname,
    triggerName: ownProps.params.triggerName
  };
};

const connector = (connect: Connect) =>
  connect<PropsFromState, unknown, ExternalProps, ReduxState>(mapStateToProps)(Container);

export default connector;
