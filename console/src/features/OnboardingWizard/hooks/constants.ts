import { growthExperimentsIds } from '@/features/GrowthExperiments';
import globals from '@/Globals';

export const experimentId = growthExperimentsIds.onboardingWizardV1;

export const graphQlMutation = `
mutation ($projectId: uuid!, $experimentId: String!, $kind: String!) {
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
