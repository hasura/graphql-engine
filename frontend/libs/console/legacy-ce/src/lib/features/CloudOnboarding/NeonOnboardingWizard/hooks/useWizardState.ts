import { useEffect, useState } from 'react';
import { getWizardState } from '../utils';
import { OnboardingResponseData } from '../../types';

export type WizardState = 'landing-page' | 'template-summary' | 'hidden';

export function useWizardState(
  onboardingData: OnboardingResponseData | undefined
) {
  const [state, setState] = useState<WizardState>(
    getWizardState(onboardingData)
  );

  useEffect(() => {
    const wizardState = getWizardState(onboardingData);
    setState(wizardState);
  }, [onboardingData]);
  return {
    state,
    setState,
  };
}
