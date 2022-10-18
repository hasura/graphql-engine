import { useEffect, useState } from 'react';
import { GrowthExperimentsClient } from '@/features/GrowthExperiments';
import { useFamiliaritySurveyData } from '@/features/Surveys';
import { experimentId } from '../constants';
import { getWizardState } from '../utils';

export type WizardState =
  | 'familiarity-survey'
  | 'landing-page'
  | 'template-summary'
  | 'hidden';

export function useWizardState(
  growthExperimentsClient: GrowthExperimentsClient,
  hasNeonAccess = false
) {
  const { getAllExperimentConfig } = growthExperimentsClient;
  const experimentData = getAllExperimentConfig();

  const {
    showFamiliaritySurvey,
    data: familiaritySurveyData,
    onSkip: familiaritySurveyOnSkip,
    onOptionClick: familiaritySurveyOnOptionClick,
  } = useFamiliaritySurveyData();

  const [state, setState] = useState<WizardState>(
    getWizardState(
      experimentData,
      experimentId,
      showFamiliaritySurvey,
      hasNeonAccess
    )
  );

  useEffect(() => {
    // this effect is only used to update the wizard state for initial async fetching of experiments config
    // it only takes care of "showing" the wizard, but not hiding it, hence the check for `hidden`
    // hiding wizard is taken care of by setting the wizard state directly to "hidden"
    const wizardState = getWizardState(
      experimentData,
      experimentId,
      showFamiliaritySurvey,
      hasNeonAccess
    );
    if (wizardState !== 'hidden') {
      setState(wizardState);
    }
  }, [experimentData, showFamiliaritySurvey, hasNeonAccess]);

  return {
    state,
    setState,
    familiaritySurveyData,
    familiaritySurveyOnSkip,
    familiaritySurveyOnOptionClick,
  };
}
