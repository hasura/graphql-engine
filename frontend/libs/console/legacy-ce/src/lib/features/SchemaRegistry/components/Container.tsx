import { useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { SchemaRegistryHome } from './SchemaRegistryHome';
import { FeatureRequest } from './FeatureRequest';
import globals from '../../../Globals';
import { SCHEMA_REGISTRY_FEATURE_NAME } from '../constants';
import { FaBell } from 'react-icons/fa';
import { Button } from '@hasura/shared/ui';
import { AlertsDialog } from './AlertsDialog';
import { Analytics, InitializeTelemetry } from '@hasura/shared/analytics';
import { useGetV2Info } from '../hooks/useGetV2Info';
import { telemetryUserEventsTracker } from '../../../telemetry';
import { useParams } from 'react-router';

export const SchemaRegistryHeader: React.FC = () => {
  const [isAlertModalOpen, setIsAlertModalOpen] = useState(false);

  return (
    <Flex direction="column" className="w-full mb-6">
      <Flex align="center" wrap="wrap" gap="3" className="mb-2 w-full">
        <h1 className="text-xl font-semibold">GraphQL Schema Registry</h1>
        <Analytics
          name="data-schema-registry-alerts-btn"
          passHtmlAttributesToChildren
        >
          <Button
            variant="outline"
            color="gray"
            leftIcon={FaBell}
            onClick={() => setIsAlertModalOpen(true)}
          >
            Configure alerts
          </Button>
        </Analytics>
      </Flex>
      <span className="text-sm" style={{ color: 'var(--gray-11)' }}>
        Explore published schemas and compare role-based changes. History is
        retained for 14 days.
      </span>
      {isAlertModalOpen && (
        <AlertsDialog onClose={() => setIsAlertModalOpen(false)} />
      )}
    </Flex>
  );
};

const SchemaRegistryBody: React.FC<{
  hasFeatureAccess: boolean;
  schemaId: string | undefined;
}> = (props) => {
  const { hasFeatureAccess, schemaId } = props;
  const projectID = globals.hasuraCloudProjectId || '';
  const v2Info = useGetV2Info(projectID);
  if (!hasFeatureAccess) {
    return <FeatureRequest />;
  }

  switch (v2Info.kind) {
    case 'loading':
      return <p>Loading...</p>;
    case 'error':
      return <p>Error: {v2Info.message}</p>;
  }

  return (
    <SchemaRegistryHome
      schemaId={schemaId}
      v2Cursor={v2Info.v2Cursor}
      v2Count={v2Info.v2Count}
    />
  );
};

export const SchemaRegistryContainer: React.FC = () => {
  const params = useParams();
  const schemaId = params.id;
  const hasFeatureAccess = globals.allowedLuxFeatures.includes(
    SCHEMA_REGISTRY_FEATURE_NAME,
  );

  return (
    <Flex
      direction="column"
      justify="center"
      className="w-full min-w-0 p-4 md:p-8"
    >
      <SchemaRegistryHeader />
      <InitializeTelemetry tracker={telemetryUserEventsTracker} skip={false} />
      <SchemaRegistryBody
        hasFeatureAccess={hasFeatureAccess}
        schemaId={schemaId}
      />
    </Flex>
  );
};
