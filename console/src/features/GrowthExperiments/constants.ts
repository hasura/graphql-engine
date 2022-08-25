/**
 * GraphQl query to fetch all growth experiments data related to the current user.
 */
export const query = `
query {
  experiments_config {
    experiment
    metadata
    status
  }
  experiments_cohort {
    experiment
    activity
  }
}
`;

export const growthExperimentsIds = {
  onboardingWizardV1: 'console_onboarding_wizard_v1',
};
