import { FeatureFlagDefinition } from './types';

const relationshipTabTablesId = '0bea35ff-d3e9-45e9-af1b-59923bf82fa9';
const remoteSchemaRelationshipsId = '9aec3960-2a52-4ab7-9660-de46cda4f3ad';

export const availableFeatureFlagIds = {
  relationshipTabTablesId,
  remoteSchemaRelationshipsId,
};

export const availableFeatureFlags: FeatureFlagDefinition[] = [
  // TODO: Uncomment this line when the actual UI for DB-to-X is ready
  // {
  //   id: relationshipTabTablesId,
  //   title: 'New Relationship tab UI for tables/views',
  //   description:
  //     'Try out the new UI for the Relationship tab of Tables/Views in Data section.',
  //   section: 'data',
  //   status: 'alpha',
  //   defaultValue: false,
  //   discussionUrl: '',
  // },
  {
    id: remoteSchemaRelationshipsId,
    title: 'Relationship tab UI for Remote schema',
    description: 'Try out the Relationship tab of Remote Schemas.',
    section: 'remote schemas',
    status: 'stable',
    defaultValue: true,
    discussionUrl: '',
  },
];
