import React, { useEffect } from 'react';
import { Flex, Heading } from '@radix-ui/themes';
import { ManageAgents } from '../ManageAgents';
import {
  Breadcrumbs,
  Button,
  Collapsible,
  IconTooltip,
  Separator,
  Text,
} from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import VPCBanner from './components/VPCBanner';
import { useVPCBannerVisibility } from './hooks/useVPCBannerVisibility';
import { ListConnectedDatabases } from '../ConnectDBRedesign';
import { useNavigate } from 'react-router';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { NeonDashboardLink } from '../CloudOnboarding/NeonOnboardingWizard/components/NeonDashboardLink';
import { useMetadata } from '@hasura/metadata/api';
import { dataRoutes } from '@hasura/shared/utils';

let autoRedirectedToConnectPage = false;

const crumbs = [
  {
    title: 'Data',
    url: `/data/`,
  },
  {
    title: 'Manage',
    url: '#',
  },
];

const ConnectedDatabaseManage: React.FC = () => {
  const navigate = useNavigate();
  const { data: sourcesFromMetadata, isFetching } = useMetadata(
    (m) => m.metadata.sources,
  );

  useDocumentTitle('Manage - Data | Hasura');

  useEffect(() => {
    if (!sourcesFromMetadata && isFetching) {
      return;
    }

    if (sourcesFromMetadata?.length === 0 && !autoRedirectedToConnectPage) {
      /**
       * Because the getDataSources() doesn't list the GDC sources, the Data tab will redirect to the /connect page
       * thinking that are no sources available in Hasura, even if there are GDC sources connected to it. Modifying getDataSources()
       * to list gdc sources is a huge task that involves modifying redux state variables.
       * So a quick workaround is to check from the actual metadata if any sources are present -
       * Combined with checks between getDataSources() and metadata -> we know the remaining sources are GDC sources. In such a case redirect to the manage db route
       */
      navigate(dataRoutes.connectDatabase());
      autoRedirectedToConnectPage = true;
    }
  }, [sourcesFromMetadata?.length, isFetching]);

  const { show: shouldShowVPCBanner, dismiss: dismissVPCBanner } =
    useVPCBannerVisibility();

  const onClickConnectDB = () => {
    navigate(dataRoutes.connectDatabase());
  };

  return (
    <Analytics name="ConnectedDatabaseManagePage" {...REDACT_EVERYTHING}>
      <div className="p-6" data-test="manage-database-section">
        <Breadcrumbs items={crumbs} />

        <div className="mt-4">
          <Flex className="manage-db-header" align="center" gap="2">
            {/* data-testid is the e2e hook the smoke test uses for the Data tab. */}
            <Heading size="4" data-testid="Data Manager">
              Data Manager
            </Heading>
            <Button mode="primary" size="1" onClick={onClickConnectDB}>
              Connect Database
            </Button>
          </Flex>
          {shouldShowVPCBanner && (
            <VPCBanner className="mt-4" onClose={dismissVPCBanner} />
          )}
        </div>
        <Separator size="4" className="my-4" />
        <ListConnectedDatabases />
        <NeonDashboardLink className="mt-6" />
        <Separator size="4" className="my-4" />
        <Collapsible
          disableContentStyles
          triggerChildren={
            <Flex align="center" gap="2">
              <Text size="3">Data Connector Agents</Text>
              <IconTooltip
                message={
                  'Data Connector Agents act as an intermediary abstraction between a data source and the Hasura GraphQL Engine.'
                }
              />
            </Flex>
          }
        >
          <ManageAgents />
        </Collapsible>
      </div>
    </Analytics>
  );
};

export default ConnectedDatabaseManage;
