import React, { useState, useEffect } from 'react';
import Helmet from 'react-helmet';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import {
  getDataEventsLandingRoute,
  getScheduledEventsLandingRoute,
  getSTRoute,
} from '../../../Common/utils/routesUtils';
import { getReactHelmetTitle } from '../../../Common/utils/reactUtils';
import tabInfo, { STTab } from './tabs';
import { findScheduledTrigger } from '../CronTriggers/utils';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix, EVENTS_SERVICE_HEADING } from '../constants';
import { setCurrentTrigger } from '../reducer';
import { Triggers } from '../types';
import { Dispatch } from '../../../../types';
import styles from '../Events.scss';

interface Props {
  triggerName: string;
  allTriggers: Triggers;
  tabName: STTab;
  dispatch: Dispatch;
  eventsLoading?: boolean;
}
type TriggerPresence =
  | 'not-missing'
  | { type: 'timeout'; timeoutHandle: number }
  | 'error-not-found';

const STContainer: React.FC<Props> = ({
  triggerName,
  children,
  allTriggers,
  tabName,
  dispatch,
  eventsLoading,
}) => {
  const [triggerPresence, setTriggerPresence] = useState<TriggerPresence>(
    'not-missing'
  );
  React.useEffect(() => {
    dispatch(setCurrentTrigger(triggerName));
    return () => {
      dispatch(setCurrentTrigger(''));
    };
  }, [triggerName]);

  const currentTrigger = findScheduledTrigger(allTriggers, triggerName);

  // TODO: This is a hack to deal with renaming cron triggers and stale props
  // https://react-redux.js.org/api/hooks#stale-props-and-zombie-children
  // Needs remodelling the state and careful handling of cron triggers rename
  useEffect(() => {
    if (currentTrigger) {
      if (
        typeof triggerPresence === 'object' &&
        triggerPresence.type === 'timeout'
      ) {
        window.clearTimeout(triggerPresence.timeoutHandle);
        setTriggerPresence('not-missing');
      }
    } else if (triggerPresence === 'not-missing') {
      const timeoutHandle = window.setTimeout(() => {
        setTriggerPresence('error-not-found');
      }, 1200 /* arbitrary value */);
      setTriggerPresence({ type: 'timeout', timeoutHandle });
    }
  }, [currentTrigger]);

  if (!currentTrigger) {
    if (eventsLoading || triggerPresence !== 'error-not-found') return null;
    throw new NotFoundError();
  }

  let activeTab = tabName as string;
  if (tabName === 'processed') {
    activeTab = 'Processed';
  } else if (tabName === 'pending') {
    activeTab = 'Pending';
  } else if (tabName === 'modify') {
    activeTab = 'Modify';
  } else if (tabName === 'logs') {
    activeTab = 'Invocation Logs';
  }

  const breadCrumbs = [
    {
      title: 'Events',
      url: getDataEventsLandingRoute(),
    },
    {
      title: 'Cron Triggers',
      url: getScheduledEventsLandingRoute(),
    },
    {
      title: triggerName,
      url: tabInfo[tabName].getRoute(triggerName),
    },
    {
      title: activeTab,
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
