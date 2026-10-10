import { useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { SchemaRegistryHome } from './SchemaRegistryHome';
import { FeatureRequest } from './FeatureRequest';
import globals from '../../../Globals';
import { SCHEMA_REGISTRY_FEATURE_NAME } from '../constants';
import { FaBell } from 'react-icons/fa';
import { IconTooltip } from '@hasura/shared/ui';
import { AlertsDialog } from './AlertsDialog';
import { Analytics, InitializeTelemetry } from '@hasura/shared/analytics';
import { useGetV2Info } from '../hooks/useGetV2Info';
import { telemetryUserEventsTracker } from '../../../telemetry';
import { useParams } from 'react-router';

const SchemaRegistryHeader: React.FC = () => {
  const [isAlertModalOpen, setIsAlertModalOpen] = useState(false);

  return (
    <Flex direction="column" className="w-full pl-12 mb-2">
      <Flex className="mb-1 mt-4 w-full">
        <h1 className="inline-block text-xl font-semibold mr-2 text-slate-900">
          GraphQL Schema Registry
        </h1>
        <Analytics name="data-schema-registry-alerts-btn">
          <Flex
            className="text-lg mt-2 mx-2 cursor-pointer"
            role="button"
            onClick={() => setIsAlertModalOpen(true)}
          >
            <IconTooltip
              message="Alerts on GraphQL schema changes"
              icon={<FaBell />}
            />
          </Flex>
        </Analytics>
      </Flex>
      <span className="text-muted text-md mb-2 italic">
        GraphQL Schema Registry changes will only be retained for 14 days.
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
    <Flex direction="column" justify="center" className="w-[80%] pl-10 ml-10">
      <SchemaRegistryHeader />
      <InitializeTelemetry tracker={telemetryUserEventsTracker} skip={false} />
      <SchemaRegistryBody
        hasFeatureAccess={hasFeatureAccess}
        schemaId={schemaId}
      />
    </Flex>
  );
};
