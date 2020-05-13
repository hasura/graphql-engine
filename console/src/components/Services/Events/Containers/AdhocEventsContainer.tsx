import React from 'react';
import Helmet from 'react-helmet';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import {
  getAdhocEventsRoute,
  getDataEventsLandingRoute,
} from '../../../Common/utils/routesUtils';
import { stripTrailingSlash } from '../../../Common/utils/urlUtils';
import tabInfo, { AdhocEventsTab } from './adhocEventsTabs';
import { appPrefix, ADHOC_EVENTS_HEADING } from '../constants';
import styles from '../Events.scss';

interface ScheduledTriggerProps extends React.ComponentProps<'div'> {
  tabName: AdhocEventsTab;
  dispatch: any;
}

const STContainer = ({ children, tabName }: ScheduledTriggerProps) => {
  const breadCrumbs = [
    {
      title: 'Events',
      url: getDataEventsLandingRoute(),
    },
    {
      title: ADHOC_EVENTS_HEADING,
      url: getAdhocEventsRoute(),
    },
    {
      title: tabName,
      url: '',
    },
  ];

  return (
    <div
      className={`${styles.view_stitch_schema_wrapper} ${styles.addWrapper}`}
    >
      <Helmet
        title={`${ADHOC_EVENTS_HEADING} | ${tabInfo[tabName].display_text}`}
      />
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab={tabName}
        heading={ADHOC_EVENTS_HEADING}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={stripTrailingSlash(getAdhocEventsRoute('absolute', ''))}
        showLoader={false}
        testPrefix="adhoc-events-container-tabs"
      />
      <div className={styles.add_pad_top}>{children}</div>
    </div>
  );
};

export default STContainer;
