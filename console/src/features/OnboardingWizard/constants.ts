import { growthExperimentsIds } from '@/features/GrowthExperiments';
import globals from '@/Globals';
import { BASE_URL_TEMPLATE } from '@/components/Services/Data/Schema/TemplateGallery/templateGalleryConfig';

// This config is stored in root level index.js, and there is a config file in each directory which stores which
// stores the directory structure.
// But in order to save api calls, we directly fetch the migraion and metadata files, without querying config.
const ROOT_DIR = 'postgres';
const TEMPLATE_DIR = 'getting-started';
export const NEON_TEMPLATE_BASE_PATH = `${BASE_URL_TEMPLATE}/${ROOT_DIR}/${TEMPLATE_DIR}`;

export const getMetadataUrl = (baseUrl: string) => {
  return `${baseUrl}/metadata.json`;
};

export const getMigrationUrl = (baseUrl: string) => {
  return `${baseUrl}/migration.sql`;
};

export const getSampleQueriesUrl = (baseUrl: string) => {
  return `${baseUrl}/sample.graphql`;
};

export const getSchemaImageUrl = (baseUrl: string) => {
  return `${baseUrl}/diagram.png`;
};

export const NEON_ONBOARDING_QUERY_KEY = 'neonOnboarding';

export const experimentId = growthExperimentsIds.onboardingWizardV1;

export const graphQlMutation = `
mutation trackExperimentsCohortActivity ($projectId: uuid!, $experimentId: String!, $kind: String!) {
  trackExperimentsCohortActivity(experiment: $experimentId, payload: {kind: $kind, project_id: $projectId}) {
    status
  }
}
`;

const projectId = globals.hasuraCloudProjectId;

const mutationVariables = {
  ...(projectId && { projectId }),
  experimentId,
};

export const onboardingCompleteVariables = {
  ...mutationVariables,
  kind: 'onboarding_complete',
};

export const skippedOnboardingVariables = {
  ...mutationVariables,
  kind: 'skipped_onboarding',
};

export const neonOAuthStartVariables = {
  ...mutationVariables,
  kind: 'neon_login_start',
};

export const neonOAuthCompleteVariables = {
  ...mutationVariables,
  kind: 'neon_login_complete',
};

export const neonDbCreationStartVariables = {
  ...mutationVariables,
  kind: 'neon_db_creation_start',
};

export const neonDbCreationCompleteVariables = {
  ...mutationVariables,
  kind: 'neon_db_creation_complete',
};

export const hasuraSourceCreationStartVariables = {
  ...mutationVariables,
  kind: 'hasura_source_creation_start',
};

export const hasuraSourceCreationCompleteVariables = {
  ...mutationVariables,
  kind: 'hasura_source_creation_complete',
};

export const installTemplateStartVariables = {
  ...mutationVariables,
  kind: 'install_template_start',
};

export const installTemplateCompleteVariables = {
  ...mutationVariables,
  kind: 'install_template_complete',
};

export const templateSummaryRunQueryClickVariables = {
  ...mutationVariables,
  kind: 'run_query_click',
};

export const templateSummaryRunQuerySkipVariables = {
  ...mutationVariables,
  kind: 'run_query_skip',
};

export const getNeonOnboardingErrorVariables = (code: string) => {
  return {
    ...mutationVariables,
    kind: 'neon_onboarding_error',
    error_code: code,
  };
};

// A stale time of 5 minutes for use in useQuery hook
export const staleTime = 5 * 60 * 1000;
