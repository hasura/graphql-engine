import React from 'react';
import { useNavigate } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { dataRoutes } from '@hasura/shared/utils';
import tabInfo, { STTab } from '../constants';
import './Events.module.scss';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useGetAllCronTriggers } from '../hooks/useGetAllCronTriggers';
import type { CronTrigger } from '@hasura/shared/types';
import { EVENTS_SERVICE_HEADING } from '@hasura/shared/types';
import { Breadcrumbs, IndicatorCard, Tabs } from '@hasura/shared/ui';
import { Heading, Skeleton } from '@radix-ui/themes';

interface Props {
  triggerName: string;
  tabName: STTab;
  children: ({
    currentTrigger,
  }: {
    currentTrigger: CronTrigger;
  }) => React.ReactNode;
}

const STContainer: React.FC<Props> = ({ triggerName, children, tabName }) => {
  const navigate = useNavigate();
  useDocumentTitle(
    dataRoutes.getReactHelmetTitle(
      `${tabInfo[tabName].display_text} - ${triggerName}`,
      EVENTS_SERVICE_HEADING,
    ),
  );

  const {
    data: currentTrigger,
    error,
    isLoading,
  } = useGetAllCronTriggers({
    select: (crons) => crons.find((cron) => cron.name === triggerName),
  });

  if (!currentTrigger) {
    if (isLoading) {
      return <Skeleton height="20px" />;
    }

    if (error) {
      return (
        <IndicatorCard status="negative" showIcon>
          There was an error, please try again later
        </IndicatorCard>
      );
    }

    if (!currentTrigger) {
      return (
        <IndicatorCard status="negative" showIcon>
          Could not find any cron trigger
        </IndicatorCard>
      );
    }
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
      url: dataRoutes.getDataEventsLandingRoute(),
    },
    {
      title: 'Cron Triggers',
      url: dataRoutes.getScheduledEventsLandingRoute(),
    },
    {
      title: triggerName,
      url: tabInfo[tabName].getRoute(triggerName),
    },
    {
      title: activeTab,
    },
  ];

  return (
    <Analytics name="CronTriggers" {...REDACT_EVERYTHING}>
      <div className="p-4">
        <Breadcrumbs items={breadCrumbs} />
        <div className="my-4">
          <Heading size="4">{triggerName}</Heading>
        </div>
        <Tabs
          value={tabName}
          onValueChange={(newTab) =>
            navigate(tabInfo[newTab as STTab].getRoute(triggerName))
          }
          items={Object.entries(tabInfo).map(([value, info]) => ({
            value,
            label: info.display_text,
            content: value === tabName ? children({ currentTrigger }) : null,
          }))}
        />
      </div>
    </Analytics>
  );
};

export default STContainer;
