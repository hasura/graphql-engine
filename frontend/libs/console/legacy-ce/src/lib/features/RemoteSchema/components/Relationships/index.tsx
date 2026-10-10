import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Tabs } from '../Tabs';
import { appPrefix } from '../../constants';
import { RemoteSchemaRelationRenderer } from './RemoteSchemaRelationRenderer';
import { useCurrentRemoteSchemaContext } from '../../context';

const RemoteSchemaRelationships = () => {
  const { currentRemoteSchema } = useCurrentRemoteSchemaContext();

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
      title: 'relationships',
      url: '',
    },
  ];

  return (
    <>
      <Tabs
        currentTab="relationships"
        heading={currentRemoteSchema.name}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${encodeURIComponent(
          currentRemoteSchema.name,
        )}`}
      />
      <Analytics name="RemoteSchemaRelationships" {...REDACT_EVERYTHING}>
        <RemoteSchemaRelationRenderer
          remoteSchemaName={currentRemoteSchema.name}
        />
      </Analytics>
    </>
  );
};

export default RemoteSchemaRelationships;
