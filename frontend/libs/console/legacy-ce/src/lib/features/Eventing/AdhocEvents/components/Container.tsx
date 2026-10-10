import React from 'react';
import { useNavigate } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { tabInfo, AdhocEventsTab } from '../constants';
import {
  ADHOC_EVENTS_HEADING,
  EVENTS_SERVICE_HEADING,
} from '@hasura/shared/types';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { dataRoutes } from '@hasura/shared/utils';
import { Breadcrumbs, Tabs } from '@hasura/shared/ui';
import { Heading } from '@radix-ui/themes';

interface Props {
  tabName: AdhocEventsTab;
  children?: React.ReactNode;
}

const STContainer: React.FC<Props> = ({ children, tabName }) => {
  const navigate = useNavigate();
  useDocumentTitle(
    dataRoutes.getReactHelmetTitle(
      `${tabInfo[tabName].display_text} - ${ADHOC_EVENTS_HEADING}`,
      EVENTS_SERVICE_HEADING,
    ),
  );

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
      url: dataRoutes.getDataEventsLandingRoute(),
    },
    {
      title: ADHOC_EVENTS_HEADING,
      url: dataRoutes.getAdhocEventsRoute(undefined),
    },
    {
      title: activeTab,
    },
  ];

  return (
    <Analytics name="AdhocEvents" {...REDACT_EVERYTHING}>
      <div className="mt-6">
        <Breadcrumbs items={breadCrumbs} />
        <div className="my-4">
          <Heading size="4">{ADHOC_EVENTS_HEADING}</Heading>
        </div>
        <Tabs
          value={tabName}
          onValueChange={(newTab) =>
            navigate(tabInfo[newTab as AdhocEventsTab].getRoute())
          }
          items={Object.entries(tabInfo).map(([value, info]) => ({
            value,
            label: info.display_text,
            content:
              value === tabName ? <div className="pt-5">{children}</div> : null,
          }))}
        />
      </div>
    </Analytics>
  );
};

export default STContainer;
