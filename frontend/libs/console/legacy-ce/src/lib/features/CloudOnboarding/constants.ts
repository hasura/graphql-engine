import globals from '../../Globals';
import { BASE_URL_TEMPLATE } from '../../components/Services/Data/Schema/TemplateGallery/templateGalleryConfig';
import { UseCases } from './UseCaseOnboarding/Root';

// This config is stored in root level index.js, and there is a config file in each directory which stores which
// stores the directory structure.
// But in order to save api calls, we directly fetch the migraion and metadata files, without querying config.
const ROOT_DIR = 'postgres';
const TEMPLATE_DIR = 'getting-started';
export const NEON_TEMPLATE_BASE_PATH = `${BASE_URL_TEMPLATE}/${ROOT_DIR}/${TEMPLATE_DIR}`;

export const onboardingQueryKey = 'onboardingData';

/**
 * GraphQl query to fetch all onboarding related data
 */
export const fetchAllOnboardingDataQuery = `
  query fetchAllOnboardingData ($projectId: uuid!) {
    user_onboarding(where: {target: {_eq: "cloud_console"}}) {
      activity
      target
      is_onboarded
    }
    users {
      id
      created_at
    }
    one_click_deployment (where: { project_id: { _eq: $projectId}}) {
      id
      state
      git_repository_url
      git_repository_branch
      hasura_directory
    }
    one_click_deployment_sample_apps {
      git_repository_url
      git_repository_branch
      hasura_directory
      name
      description
      rank
      react_icons_fa_component_name
    }
  }
`;

export const fetchAllOnboardingDataQueryVariables = {
  projectId: globals.hasuraCloudProjectId,
};

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

export const trackOnboardingActivityMutation = `
  mutation trackOnboardingActivity($projectId: uuid!, $kind: String!, $error_code: String, $subkind: String,) {
    trackOnboardingActivity(payload: {kind: $kind, project_id: $projectId, error_code: $error_code, subkind: $subkind}) {
      status
    }
}
`;

const projectId = globals.hasuraCloudProjectId;

const mutationVariables = {
  ...(projectId && { projectId }),
};

export const skippedOnboardingThroughURLParamVariables = {
  ...mutationVariables,
  kind: 'skipped_neon_onboarding_through_url_param',
};
export const skippedNeonOnboardingToConnectOtherDB = {
  ...mutationVariables,
  kind: 'skipped_neon_onboarding_to_connect_other_db',
};

export const skippedNeonOnboardingVariables = {
  ...mutationVariables,
  kind: 'skipped_onboarding',
};

export const neonOAuthStartVariables = {
  ...mutationVariables,
  kind: 'neon_login_start',
};

export const neonDbCreationStartVariables = {
  ...mutationVariables,
  kind: 'neon_db_creation_start',
};

export const hasuraSourceCreationStartVariables = {
  ...mutationVariables,
  kind: 'hasura_source_creation_start',
};

export const installTemplateStartVariables = {
  ...mutationVariables,
  kind: 'install_template_start',
};

export const templateSummaryRunQueryClickVariables = {
  ...mutationVariables,
  kind: 'run_query_click',
};

export const templateSummaryRunQuerySkipVariables = {
  ...mutationVariables,
  kind: 'run_query_skip',
};

export const oneClickDeploymentOnboardingShown = {
  ...mutationVariables,
  kind: 'onboarded_through_one_click_deployment',
};

export const useCaseExperimentOnboarding = {
  ...mutationVariables,
  kind: 'onboarded_through_use_case_experiment',
};

export const skippedUseCaseExperimentOnboarding = {
  ...mutationVariables,
  kind: 'skipped_use_case_experiment_onboarding',
};

export const getUseCaseExperimentOnboardingVariables = (useCase: UseCases) => ({
  ...useCaseExperimentOnboarding,
  subkind: useCase,
});

export const getNeonOnboardingErrorVariables = (code: string) => {
  return {
    ...mutationVariables,
    kind: 'neon_onboarding_error',
    error_code: code,
  };
};

// A stale time of 5 minutes for use in useQuery hook
export const staleTime = 5 * 60 * 1000;

export const dialogHeader = 'Welcome to your new Hasura project!';

export const familiaritySurveySubHeader =
  "We'd love to get to know you before you get started with your first API.";

export const stepperNavSteps = [
  {
    step: '01',
    text: 'Getting Started',
  },
  {
    step: '02',
    text: 'Connect Database',
  },
  {
    step: '03',
    text: 'Make Your First Query',
  },
];
