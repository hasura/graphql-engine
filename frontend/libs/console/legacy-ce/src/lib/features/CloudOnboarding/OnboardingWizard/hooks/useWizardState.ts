import { useEffect, useState } from 'react';
import { useSurveysData } from '../../../Surveys';
import { trackCustomEvent } from '../../../Analytics';
import { useOnboardingData } from './useOnboardingData';
import { getWizardState } from '../utils';
import { AllowedSurveyNames } from '../../../Surveys/types';

export type WizardState =
  | 'familiarity-survey'
  | 'landing-page'
  | 'template-summary'
  | 'use-case-onboarding'
  | 'hidden';

export function useWizardState() {
  useEffect(() => {
    trackCustomEvent({
      location: 'Console',
      action: 'Load',
      object: 'Onboarding Wizard',
    });
  }, []);

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
    setState(wizardState);
  }, [onboardingData, showSurvey]);

  return {
    state,
    setState,
    familiaritySurveyData,
    familiaritySurveyOnOptionClick,
  };
}
