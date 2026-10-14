import { useState } from 'react';
import { getWizardState } from '../utils';
import { OnboardingResponseData } from '../../types';

export type WizardState = 'landing-page' | 'template-summary' | 'hidden';

export function useWizardState(
  onboardingData: OnboardingResponseData | undefined,
) {
  const [state, setState] = useState<WizardState>(() =>
    getWizardState(onboardingData),
  );

  // `state` is derived from `onboardingData` but is also manually controllable
  // by consumers (e.g. to transition the wizard between steps). Rather than
  // re-syncing from props inside an effect — which triggers an extra render —
  // we store the previous `onboardingData` and recompute during render when it
  // changes (React's "adjusting state while a prop changes" pattern).
  const [prevOnboardingData, setPrevOnboardingData] = useState(onboardingData);
  if (onboardingData !== prevOnboardingData) {
    setPrevOnboardingData(onboardingData);
    setState(getWizardState(onboardingData));
  }

  return {
    state,
    setState,
  };
}
