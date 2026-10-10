import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Tabs } from '../Tabs';
import { appPrefix } from '../../constants';
import { useCurrentRemoteSchemaContext } from '../../context';
import Permissions from './Permissions';
import { useServerConfig } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { Code } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';

const tabName = 'permissions';

const RemoteSchemaPermissions: React.FC = () => {
  const { data: rspEnabled } = useServerConfig(
    (config) => config.is_remote_schema_permissions_enabled,
  );
  const { currentRemoteSchema, metadata } = useCurrentRemoteSchemaContext();

  const breadCrumbs = [
    {
      title: 'Remote schemas',
      url: appPrefix,
    },
    {
      title: 'Manage',
      url: `${appPrefix}/manage`,
    },
    {
      title: currentRemoteSchema.name,
      url: `${appPrefix}/manage/${encodeURIComponent(
        currentRemoteSchema.name,
      )}/modify`,
    },
    {
      title: tabName,
      url: '',
    },
  ];

  return (
    <>
      <Tabs
        currentTab={tabName}
        heading={currentRemoteSchema.name}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${currentRemoteSchema.name}`}
      />
      <Analytics name="RemoteSchemaPermission" {...REDACT_EVERYTHING}>
        <div>
          {rspEnabled ? (
            <Permissions
              allRoles={MetadataSelectors.getRoles(metadata)}
              currentRemoteSchema={currentRemoteSchema}
            />
          ) : (
            <Text>
              Remote schema permissions are not enabled. To enable remote schema
              permissions, start the Hasura server with environment variable{' '}
              <Code>
                HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS:
                &quot;true&quot;
              </Code>
            </Text>
          )}
        </div>
      </Analytics>
    </>
  );
};

export default RemoteSchemaPermissions;
