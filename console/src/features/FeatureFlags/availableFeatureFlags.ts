import { FeatureFlagDefinition } from './types';

const relationshipTabTablesId = '0bea35ff-d3e9-45e9-af1b-59923bf82fa9';
const gdcId = '88436c32-2798-11ed-a261-0242ac120002';
const allowListId = '3a042e0c-e0d4-46b6-8c87-01f2ed9f63b0';

export const availableFeatureFlagIds = {
  relationshipTabTablesId,
  gdcId,
  allowListId,
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
    id: allowListId,
    title: 'New Allow List Manager',
    description:
      'Try out the new UI for the Allow List management in the API section.',
    section: 'api',
    status: 'alpha',
    defaultValue: false,
    discussionUrl: '',
  },
  {
    id: gdcId,
    title: 'Experimental features for GDC',
    description:
      'Try out the very experimental features that are available for GDC on the console',
    section: 'data',
    status: 'experimental',
    defaultValue: false,
    discussionUrl: '',
  },
];
