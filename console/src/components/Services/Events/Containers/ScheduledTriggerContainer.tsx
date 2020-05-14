import React from 'react';
import Helmet from 'react-helmet';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import {
  getDataEventsLandingRoute,
  getScheduledEventsLandingRoute,
  getSTRoute,
} from '../../../Common/utils/routesUtils';
import { getReactHelmetTitle } from '../../../Common/utils/reactUtils';
import tabInfo, { STTab } from './scheduledTriggerTabs';
import { findScheduledTrigger } from '../ScheduledTriggers/utils';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix, EVENTS_SERVICE_HEADING } from '../constants';
import { setCurrentTrigger } from '../reducer';
import { Triggers } from '../types';
import { Dispatch } from '../../../../types';
import styles from '../Events.scss';

interface ScheduledTriggerProps extends React.ComponentProps<'div'> {
  triggerName: string;
  allTriggers: Triggers;
  tabName: STTab;
  dispatch: Dispatch;
}

const STContainer = ({
  triggerName,
  children,
  allTriggers,
  tabName,
  dispatch,
}: ScheduledTriggerProps) => {
  React.useEffect(() => {
    dispatch(setCurrentTrigger(triggerName));
    return () => {
      dispatch(setCurrentTrigger(''));
    };
  }, [triggerName]);

  const currentTrigger = findScheduledTrigger(allTriggers, triggerName);

  if (!currentTrigger) {
    dispatch(setCurrentTrigger(''));
    throw new NotFoundError();
  }

  const breadCrumbs = [
    {
      title: 'Events',
      url: getDataEventsLandingRoute(),
    },
    {
      title: 'Scheduled',
      url: getScheduledEventsLandingRoute(),
    },
    {
      title: triggerName,
      url: tabInfo[tabName].getRoute(triggerName),
    },
    {
      title: tabName,
      url: '',
    },
  ];

  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child as React.ReactElement<any>, {
      currentTrigger,
    })
  );

  return (
    <div
      className={`${styles.view_stitch_schema_wrapper} ${styles.addWrapper}`}
    >
      <Helmet
        title={getReactHelmetTitle(
          `${tabInfo[tabName].display_text} - ${triggerName}`,
          EVENTS_SERVICE_HEADING
        )}
      />
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab={tabName}
        heading={triggerName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={getSTRoute('absolute', triggerName)}
        showLoader={false}
        testPrefix={`${triggerName}-container-tabs`}
      />
      <div className={styles.add_pad_top}>{childrenWithProps}</div>
    </div>
  );
};

export default STContainer;
