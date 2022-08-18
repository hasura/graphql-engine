import { FeatureFlagDefinition } from './types';

const relationshipTabTablesId = '0bea35ff-d3e9-45e9-af1b-59923bf82fa9';
const addRemoteSchemaId = 'bf57c2ba-cab2-11ec-9d64-0242ac120002';

export const availableFeatureFlagIds = {
  relationshipTabTablesId,
  addRemoteSchemaId,
};

export const availableFeatureFlags: FeatureFlagDefinition[] = [
  {
    id: relationshipTabTablesId,
    title: 'New Relationship tab UI for tables/views',
    description:
      'Try out the new UI for the Relationship tab of Tables/Views in Data section.',
    section: 'data',
    status: 'alpha',
    defaultValue: false,
    discussionUrl: '',
  },
  {
    id: addRemoteSchemaId,
    title: 'New create remote schema page',
    description:
      'Try out the new Add Remote Schema page that supports GraphQL customization',
    section: 'remote schemas',
    status: 'alpha',
    defaultValue: false,
    discussionUrl: '',
  },
];
