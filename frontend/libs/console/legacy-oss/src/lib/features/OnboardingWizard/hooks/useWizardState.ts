import { useEffect, useState } from 'react';
import { GrowthExperimentsClient } from '@/features/GrowthExperiments';
import { cloudDataServiceApiClient } from '@/hooks/cloudDataServiceApiClient';
import {
  experimentId,
  graphQlMutation,
  onboardingCompleteVariables,
  skippedOnboardingVariables,
} from './constants';
import { isExperimentActive, shouldShowOnboarding } from './utils';

type ResponseDataOnMutation = {
  data: {
    trackExperimentsCohortActivity: {
      status: string;
    };
  };
};

export function useWizardState(
  growthExperimentsClient: GrowthExperimentsClient
) {
  // lux works with cookie auth and doesn't require admin-secret header
  const headers = {
    'content-type': 'application/json',
  };

  const { getAllExperimentConfig, setAllExperimentConfig } =
    growthExperimentsClient;
  const experimentData = getAllExperimentConfig();

  const [isWizardOpen, setIsWizardOpen] = useState(
    shouldShowOnboarding(experimentData, experimentId) &&
      isExperimentActive(experimentData, experimentId)
  );

  useEffect(() => {
    setIsWizardOpen(
      shouldShowOnboarding(experimentData, experimentId) &&
        isExperimentActive(experimentData, experimentId)
    );
  }, [experimentData]);

  const skipOnboarding = () => {
    setIsWizardOpen(false);

    // mutate server data
    cloudDataServiceApiClient<ResponseDataOnMutation, ResponseDataOnMutation>(
      graphQlMutation,
      skippedOnboardingVariables,
      headers
    )
      .then(() => {
        // refetch the fresh data and update the `growthExperimentConfigClient`
        setAllExperimentConfig();
      })
      .catch(error => {
        console.error(error);
      });
  };

  const completeOnboarding = () => {
    setIsWizardOpen(false);

    // mutate server data
    cloudDataServiceApiClient<ResponseDataOnMutation, ResponseDataOnMutation>(
      graphQlMutation,
      onboardingCompleteVariables,
      headers
    )
      .then(() => {
        // refetch the fresh data and update the `growthExperimentConfigClient`
        setAllExperimentConfig();
      })
      .catch(error => {
        console.error(error);
      });
  };

  return { isWizardOpen, skipOnboarding, completeOnboarding };
}
