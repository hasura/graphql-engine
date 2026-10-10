import React from 'react';
import { ConnectDBScreen, TemplateSummary } from './components';
import { DialogContainer } from '../components';

import { useWizardState } from './hooks';
import {
  dialogHeader,
  NEON_TEMPLATE_BASE_PATH,
  stepperNavSteps,
} from '../constants';
import { OnboardingResponseData } from '../types';

/**
 * Parent container for the onboarding wizard. Takes care of assembling and rendering all steps.
 */
export function Root(props: {
  onboardingData: OnboardingResponseData | undefined;
}) {
  const [stepperIndex, setStepperIndex] = React.useState<number>(1);
  const { state, setState } = useWizardState(props.onboardingData);

  const transitionToTemplateSummary = () => {
    setState('template-summary');
  };

  const dismiss = () => {
    setState('hidden');
  };

  switch (state) {
    case 'landing-page': {
      return (
        <DialogContainer
          showStepper
          stepperNavSteps={stepperNavSteps}
          activeIndex={stepperIndex}
          header={dialogHeader}
        >
          <ConnectDBScreen
            dismissOnboarding={dismiss}
            proceed={transitionToTemplateSummary}
            setStepperIndex={setStepperIndex}
          />
        </DialogContainer>
      );
    }
    case 'template-summary': {
      return (
        <DialogContainer
          showStepper
          stepperNavSteps={stepperNavSteps}
          activeIndex={3}
          header={dialogHeader}
        >
          <TemplateSummary
            templateUrl={NEON_TEMPLATE_BASE_PATH}
            dismiss={dismiss}
          />
        </DialogContainer>
      );
    }
    case 'hidden':
    default: {
      return null;
    }
  }
}
