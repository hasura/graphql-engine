import { useEffect, useState } from 'react';
import { GrowthExperimentsClient } from '@/features/GrowthExperiments';
import { experimentId } from '../constants';
import { isExperimentActive, shouldShowOnboarding } from '../utils';

type WizardState = 'landing-page' | 'template-summary' | 'hidden';

export function useWizardState(
  growthExperimentsClient: GrowthExperimentsClient,
  hasNeonAccess = false
) {
  const { getAllExperimentConfig } = growthExperimentsClient;
  const experimentData = getAllExperimentConfig();

  const [state, setState] = useState<WizardState>(
    shouldShowOnboarding(experimentData, experimentId, hasNeonAccess) &&
      isExperimentActive(experimentData, experimentId)
      ? 'landing-page'
      : 'hidden'
  );

  useEffect(() => {
    // this effect is only used to update the wizard state for initial async fetching of experiments config
    // it only takes care of "showing" the wizard, but not hiding it, hence the check for `hidden`
    // hiding wizard is taken care of by setting the wizard state directly to "hidden"
    const wizardState =
      shouldShowOnboarding(experimentData, experimentId, hasNeonAccess) &&
      isExperimentActive(experimentData, experimentId)
        ? 'landing-page'
        : 'hidden';
    if (wizardState !== 'hidden') {
      setState(wizardState);
    }
  }, [growthExperimentsClient.getAllExperimentConfig()]);

  return { state, setState };
}
