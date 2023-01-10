import { isProConsole } from '@/utils';
import { FeatureFlagDefinition } from './types';

const relationshipTabTablesId = '0bea35ff-d3e9-45e9-af1b-59923bf82fa9';
const gdcId = '88436c32-2798-11ed-a261-0242ac120002';
const importActionFromOpenApiId = '12e5aaf4-c794-4b8f-b762-5fda0bff946a';

export const availableFeatureFlagIds = {
  relationshipTabTablesId,
  gdcId,
  importActionFromOpenApiId,
};

const importActionFromOpenApi: FeatureFlagDefinition = {
  id: importActionFromOpenApiId,
  title: 'Import Action from OpenAPI',
  description:
    'Try out the very experimental feature to generate one action from an OpenAPI endpoint',
  section: 'data',
  status: 'experimental',
  defaultValue: false,
  discussionUrl: '',
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
    id: gdcId,
    title: 'Turn on experimental features',
    description:
      'Try out the really experimental release features. This includes the new database connections for Amazon Athena and Snowflake at this time.',
    section: 'data',
    status: 'experimental',
    defaultValue: false,
    discussionUrl: '',
  },
  // eslint-disable-next-line no-underscore-dangle
  ...(isProConsole(window.__env) ? [importActionFromOpenApi] : []),
];
