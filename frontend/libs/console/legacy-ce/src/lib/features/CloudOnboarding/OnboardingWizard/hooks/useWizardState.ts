import { useEffect, useState } from 'react';
import { useSurveysData } from '../../../Surveys';
import { useOnboardingData } from './useOnboardingData';
import { getWizardState } from '../utils';
import { AllowedSurveyNames } from '../../../Surveys/types';
import { getLSItem, LS_KEYS, removeLSItem, setLSItem } from '../../../../utils';

export type WizardState =
  | 'familiarity-survey'
  | 'landing-page'
  | 'template-summary'
  | 'use-case-onboarding'
  | 'hidden';

export function useWizardState() {
  const {
    show: showSurvey,
    data: familiaritySurveyData,
    onSubmit: familiaritySurveyOnOptionClick,
  } = useSurveysData({ surveyName: AllowedSurveyNames.familiaritySurvey });

  const { data: onboardingData } = useOnboardingData();

  const [state, setState] = useState<WizardState>(
    getWizardState(showSurvey, onboardingData)
  );

  useEffect(() => {
    const wizardState = getWizardState(showSurvey, onboardingData);
    if (wizardState !== 'hidden') {
      setLSItem(LS_KEYS.showUseCaseOverviewPopup, 'true');
    }
    setState(wizardState);

    //removing skipOnboarding key from LocalStorage
    if (getLSItem(LS_KEYS.skipOnboarding) === 'true') {
      removeLSItem(LS_KEYS.skipOnboarding);
    }
  }, [onboardingData, showSurvey]);
  return {
    state,
    setState,
    familiaritySurveyData,
    familiaritySurveyOnOptionClick,
  };
}
