import React from 'react';
import { useAppDispatch } from '../../../storeHooks';
import { AllowedSurveyThemes, Survey } from '../../Surveys';
import {
  ConnectDBScreen,
  TemplateSummary,
  DialogContainer,
  UseCaseScreen,
} from './components';

import { useWizardState } from './hooks';
import {
  NEON_TEMPLATE_BASE_PATH,
  dialogHeader,
  familiaritySurveySubHeader,
  stepperNavSteps,
} from './constants';

/**
 * Parent container for the onboarding wizard. Takes care of assembling and rendering all steps.
 */
export function Root() {
  const dispatch = useAppDispatch();

  const [stepperIndex, setStepperIndex] = React.useState<number>(1);

  const {
    state,
    setState,
    familiaritySurveyData,
    familiaritySurveyOnOptionClick,
  } = useWizardState();

  const transitionToTemplateSummary = () => {
    setState('template-summary');
  };

  const dismiss = () => {
    setState('hidden');
  };

  switch (state) {
    case 'familiarity-survey': {
      return (
        <DialogContainer
          header={dialogHeader}
          subHeader={familiaritySurveySubHeader}
        >
          <Survey
            theme={AllowedSurveyThemes.familiaritySurveyTheme}
            onSubmit={familiaritySurveyOnOptionClick}
            data={familiaritySurveyData}
          />
        </DialogContainer>
      );
    }
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
            dispatch={dispatch}
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
            dispatch={dispatch}
          />
        </DialogContainer>
      );
    }
    case 'use-case-onboarding':
      return (
        <DialogContainer header="">
          <UseCaseScreen dismiss={dismiss} dispatch={dispatch} />
        </DialogContainer>
      );
    case 'hidden':
    default: {
      return null;
    }
  }
}
