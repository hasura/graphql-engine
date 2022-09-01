import { ExperimentConfig } from '@/features/GrowthExperiments';

export function isExperimentActive(
  experimentsData: ExperimentConfig[],
  experimentId: string
) {
  const experimentData = experimentsData?.find(
    experimentConfig => experimentConfig.experiment === experimentId
  );
  return experimentData && experimentData?.status === 'enabled';
}

export function shouldShowOnboarding(
  experimentsData: ExperimentConfig[],
  experimentId: string
) {
  const experimentData = experimentsData?.find(
    experimentConfig => experimentConfig.experiment === experimentId
  );
  if (
    experimentData?.userActivity?.onboarding_complete ||
    experimentData?.userActivity?.skipped_onboarding
  ) {
    return false;
  }
  return true;
}
