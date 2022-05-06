import { FeatureFlagDefinition } from './types';

const relationshipTabTablesId = '0bea35ff-d3e9-45e9-af1b-59923bf82fa9';

export const availableFeatureFlagIds = {
  relationshipTabTablesId,
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
];
