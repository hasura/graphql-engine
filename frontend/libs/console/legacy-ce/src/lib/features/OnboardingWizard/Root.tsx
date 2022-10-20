import React from 'react';
import { useAppDispatch } from '@/store';
import globals from '@/Globals';
import { hasLuxFeatureAccess, isCloudConsole } from '@/utils/cloudConsole';
import {
  ConnectDBScreen,
  TemplateSummary,
  DialogContainer,
} from './components';

import { useWizardState } from './hooks';
import {
  NEON_TEMPLATE_BASE_PATH,
  dialogHeader,
  familiaritySurveySubHeader,
} from './constants';
import { GrowthExperimentsClient } from '../GrowthExperiments';
import { HasuraFamiliaritySurvey } from '../Surveys';

type Props = {
  growthExperimentsClient: GrowthExperimentsClient;
};

/**
 * Parent container for the onboarding wizard. Takes care of assembling and rendering all steps.
 */
function Root(props: Props) {
  const { growthExperimentsClient } = props;

  const dispatch = useAppDispatch();

  const hasNeonAccess = hasLuxFeatureAccess(globals, 'NeonDatabaseIntegration');

  const [stepperIndex, setStepperIndex] = React.useState<number>(1);

  const {
    state,
    setState,
    familiaritySurveyData,
    familiaritySurveyOnOptionClick,
    familiaritySurveyOnSkip,
  } = useWizardState(growthExperimentsClient, hasNeonAccess);

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
          <HasuraFamiliaritySurvey
            data={familiaritySurveyData}
            onSkip={familiaritySurveyOnSkip}
            onOptionClick={familiaritySurveyOnOptionClick}
          />
        </DialogContainer>
      );
    }
    case 'landing-page': {
      return (
        <DialogContainer
          showStepper
          activeIndex={stepperIndex}
          header={dialogHeader}
        >
          <ConnectDBScreen
            dismissOnboarding={dismiss}
            proceed={transitionToTemplateSummary}
            hasNeonAccess={hasNeonAccess}
            dispatch={dispatch}
            setStepperIndex={setStepperIndex}
          />
        </DialogContainer>
      );
    }
    case 'template-summary': {
      return (
        <DialogContainer showStepper activeIndex={3} header={dialogHeader}>
          <TemplateSummary
            templateUrl={NEON_TEMPLATE_BASE_PATH}
            dismiss={dismiss}
            dispatch={dispatch}
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

export function RootWithCloudCheck(props: Props) {
  /*
   * Don't render Root component if current context is not cloud-console
   * and current user is not project owner
   */
  if (isCloudConsole(globals) && globals.userRole === 'owner') {
    return <Root {...props} />;
  }
  return null;
}

export const RootWithoutCloudCheck = Root;
