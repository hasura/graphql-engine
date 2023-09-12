import { useState } from 'react';
import { SchemaRegistryHome } from './SchemaRegistryHome';
import { FeatureRequest } from './FeatureRequest';
import globals from '../../../Globals';
import { SCHEMA_REGISTRY_FEATURE_NAME } from '../constants';
import { FaBell } from 'react-icons/fa';
import { IconTooltip } from '../../../new-components/Tooltip';
import { AlertsDialog } from './AlertsDialog';
import { Badge } from '../../../new-components/Badge';
import { SCHEMA_REGISTRY_REF_URL } from '../constants';
import { Analytics } from '../../Analytics';
import { useGetV2Info } from '../hooks/useGetV2Info';

const SchemaRegistryHeader: React.VFC = () => {
  const [isAlertModalOpen, setIsAlertModalOpen] = useState(false);

  return (
    <div className="flex flex-col w-full">
      <div className="flex mb-sm mt-md w-full">
        <h1 className="inline-block text-xl font-semibold mr-2 text-slate-900">
          GraphQL Schema Registry
        </h1>
        <Badge className="mx-2" color="blue">
          BETA
        </Badge>
        <Analytics name="data-schema-registry-alerts-btn">
          <div
            className="flex text-lg mt-2 mx-2 cursor-pointer"
            role="button"
            onClick={() => setIsAlertModalOpen(true)}
          >
            <IconTooltip
              message="Alerts on GraphQL schema changes"
              icon={<FaBell />}
            />
          </div>
        </Analytics>
      </div>
      <a
        className="text-muted w-auto mb-md"
        href={SCHEMA_REGISTRY_REF_URL}
        target="_blank"
        rel="noreferrer noopener"
      >
        What is Schema Registry?
      </a>
      {isAlertModalOpen && (
        <AlertsDialog onClose={() => setIsAlertModalOpen(false)} />
      )}
    </div>
  );
};

const SchemaRegistryBody: React.VFC<{
  hasFeatureAccess: boolean;
  schemaId: string | undefined;
}> = props => {
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

type SchemaDetailsViewProps = {
  params: {
    id?: string;
  };
};

export const SchemaRegistryContainer: React.VFC<
  SchemaDetailsViewProps
> = props => {
  const { id: schemaId } = props.params;
  const hasFeatureAccess = globals.allowedLuxFeatures.includes(
    SCHEMA_REGISTRY_FEATURE_NAME
  );

  return (
    <div className="p-4 flex flex-col w-full">
      <SchemaRegistryHeader />
      <SchemaRegistryBody
        hasFeatureAccess={hasFeatureAccess}
        schemaId={schemaId}
      />
    </div>
  );
};
