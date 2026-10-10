import { useNavigate } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { dataRoutes } from '@hasura/shared/utils';
import { BreadcrumbItem, Breadcrumbs, Tabs } from '@hasura/shared/ui';
import { EVENTS_SERVICE_HEADING } from '@hasura/shared/types';
import { Heading } from '@radix-ui/themes';

type Props = {
  triggerName: string;
  tabName: string;
  count?: number | null;
  readOnlyMode: boolean;
};

const TableHeader = ({ triggerName, tabName, readOnlyMode }: Props) => {
  const navigate = useNavigate();

  let activeTab = '';
  if (tabName === 'processed') {
    activeTab = 'Processed';
  } else if (tabName === 'pending') {
    activeTab = 'Pending';
  } else if (tabName === 'modify') {
    activeTab = 'Modify';
  } else if (tabName === 'logs') {
    activeTab = 'Invocation Logs';
  }

  useDocumentTitle(
    dataRoutes.getReactHelmetTitle(
      `${activeTab} - ${triggerName}`,
      EVENTS_SERVICE_HEADING,
    ),
  );

  const breadCrumbs: BreadcrumbItem[] = [
    {
      title: 'Events',
      url: '',
    },
    {
      title: 'Data Triggers',
      url: dataRoutes.getDataEventsLandingRoute(),
    },
    {
      title: triggerName,
      url: dataRoutes.getETProcessedEventsRoute(triggerName),
    },
    {
      title: activeTab,
    },
  ];

  return (
    <div>
      <Analytics name="EventsTableHeader" {...REDACT_EVERYTHING}>
        <div>
          <Breadcrumbs className="py-4" items={breadCrumbs} />
          <Heading size="4">{triggerName}</Heading>
          <Tabs
            className="mt-2"
            color="indigo"
            value={location.pathname}
            onValueChange={(value) => navigate(value)}
            items={(!readOnlyMode
              ? [
                  {
                    value: dataRoutes.getETModifyRoute({ name: triggerName }),
                    label: 'Modify',
                  },
                ]
              : []
            ).concat([
              {
                value: `/events/data/${triggerName}/pending`,
                label: 'Pending Events',
              },
              {
                value: `/events/data/${triggerName}/processed`,
                label: 'Processed Events',
              },
              {
                value: `/events/data/${triggerName}/logs`,
                label: 'Invocation Logs',
              },
            ])}
          />
        </div>
      </Analytics>
    </div>
  );
};
export default TableHeader;
