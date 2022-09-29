import { growthExperimentsIds } from '@/features/GrowthExperiments';
import globals from '@/Globals';
import { BASE_URL_TEMPLATE } from '@/components/Services/Data/Schema/TemplateGallery/templateGalleryConfig';

// This config is stored in root level index.js, and there is a config file in each directory which stores which
// stores the directory structure.
// But in order to save api calls, we directly fetch the migraion and metadata files, without querying config.
const ROOT_DIR = 'postgres';
const TEMPLATE_DIR = 'getting-started';
const NEON_TEMPLATE_BASE_PATH = `${BASE_URL_TEMPLATE}/${ROOT_DIR}/${TEMPLATE_DIR}`;

export const NEON_METADATA_PATH = `${NEON_TEMPLATE_BASE_PATH}/metadata.json`;
export const NEON_MIGRATIONS_PATH = `${NEON_TEMPLATE_BASE_PATH}/migration.sql`;
export const NEON_QUERY_PATH = `${NEON_TEMPLATE_BASE_PATH}/sample.graphql`;
export const NEON_IMAGE_PATH = `${NEON_TEMPLATE_BASE_PATH}/diagram.png`;

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

export const hasuraSourceCreationStart = {
  ...mutationVariables,
  kind: 'hasura_source_creation_start',
};

export const hasuraSourceCreationComplete = {
  ...mutationVariables,
  kind: 'hasura_source_creation_complete',
};

export const templateSummaryRunQueryClick = {
  ...mutationVariables,
  kind: 'run_query_click',
};

export const templateSummaryRunQuerySkip = {
  ...mutationVariables,
  kind: 'run_query_skip',
};

// A stale time of 5 minutes for use in useQuery hook
export const staleTime = 5 * 60 * 1000;
