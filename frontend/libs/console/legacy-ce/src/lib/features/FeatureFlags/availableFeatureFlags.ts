import { FeatureFlagDefinition } from './types';
import { isProConsole } from '../../utils/proConsole';
import globals from '../../Globals';

const relationshipTabTablesId = 'f6c57c31-abd3-46d9-aae9-b97435793273';
const importActionFromOpenApiId = '12e5aaf4-c794-4b8f-b762-5fda0bff946a';
const trackingSectionUI = 'c2536b28-0ea3-11ee-be56-0242ac120002';
const performanceMode = '29ea2999-b1fe-4c0d-a2ba-113098521919';

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

export const availableFeatureFlagIds = {
  relationshipTabTablesId,
  importActionFromOpenApiId,
  trackingSectionUI,
  performanceMode,
};

export const availableFeatureFlags: FeatureFlagDefinition[] = [
  {
    id: relationshipTabTablesId,
    title: 'New Relationship tab UI for tables/views',
    description:
      'Use the new UI for the Relationship tab of Tables/Views in Data section.',
    section: 'data',
    status: 'release candidate',
    defaultValue: true,
    discussionUrl: '',
  },
  {
    id: trackingSectionUI,
    title: 'Enable new Table Tracking UI for Postgres & SQL Server',
    description: 'Try out the new UI experience for tracking tables',
    section: 'data',
    status: 'alpha',
    defaultValue: false,
    discussionUrl: 'https://github.com/hasura/graphql-engine/discussions/9727',
  },
  {
    id: performanceMode,
    title: 'Data Tab Performance Mode',
    description:
      'Enables high performance components in the Data tab. Some database features are not available.',
    section: 'data',
    status: 'alpha',
    defaultValue: false,
    discussionUrl: '',
  },
  // eslint-disable-next-line no-underscore-dangle
  ...(isProConsole(globals) ? [importActionFromOpenApi] : []),
];
