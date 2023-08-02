import React from 'react';
import Helmet from 'react-helmet';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import {
  getAdhocEventsRoute,
  getDataEventsLandingRoute,
} from '../../../Common/utils/routesUtils';
import { stripTrailingSlash } from '../../../Common/utils/urlUtils';
import { getReactHelmetTitle } from '../../../Common/utils/reactUtils';
import tabInfo, { AdhocEventsTab } from './tabs';
import {
  appPrefix,
  ADHOC_EVENTS_HEADING,
  EVENTS_SERVICE_HEADING,
} from '../constants';
import { Dispatch } from '../../../../types';

interface Props {
  tabName: AdhocEventsTab;
  dispatch: Dispatch;
}

const STContainer: React.FC<Props> = ({ children, tabName }) => {
  let activeTab = tabName as string;
  if (tabName === 'processed') {
    activeTab = 'Processed';
  } else if (tabName === 'pending') {
    activeTab = 'Pending';
  } else if (tabName === 'add') {
    activeTab = 'Create';
  } else if (tabName === 'logs') {
    activeTab = 'Invocation Logs';
  } else if (tabName === 'info') {
    activeTab = 'Info';
  }

  const breadCrumbs = [
    {
      title: 'Events',
      url: getDataEventsLandingRoute(),
    },
    {
      title: ADHOC_EVENTS_HEADING,
      url: getAdhocEventsRoute(undefined),
    },
    {
      title: activeTab,
      url: '',
    },
  ];

  return (
    <Analytics name="AdhocEvents" {...REDACT_EVERYTHING}>
      <div>
        <Helmet
          title={getReactHelmetTitle(
            `${tabInfo[tabName].display_text} - ${ADHOC_EVENTS_HEADING}`,
            EVENTS_SERVICE_HEADING
          )}
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
        <div className="pt-5">{children}</div>
      </div>
    </Analytics>
  );
};

export default STContainer;
