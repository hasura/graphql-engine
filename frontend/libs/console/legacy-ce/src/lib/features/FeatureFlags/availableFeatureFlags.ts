import { isProConsole } from '@/utils/proConsole';
import { FeatureFlagDefinition } from './types';

const relationshipTabTablesId = '0bea35ff-d3e9-45e9-af1b-59923bf82fa9';
const importActionFromOpenApiId = '12e5aaf4-c794-4b8f-b762-5fda0bff946a';
const enabledNewUIForBigQuery = 'e2d790ba-96fb-11ed-a8fc-0242ac120002';

export const availableFeatureFlagIds = {
  relationshipTabTablesId,
  importActionFromOpenApiId,
  enabledNewUIForBigQuery,
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
    id: enabledNewUIForBigQuery,
    title: 'Enable the revamped UI for BigQuery',
    description: 'Try out the new UI experience for BigQuery.',
    section: 'data',
    status: 'experimental',
    defaultValue: false,
    discussionUrl: '',
  },
];
