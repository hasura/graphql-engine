import React from 'react';
import * as Dialog from '@radix-ui/react-dialog';
import { useAppDispatch } from '@/store';
import globals from '@/Globals';
import { hasLuxFeatureAccess, isCloudConsole } from '@/utils/cloudConsole';
import { TopHeaderBar, ConnectDBScreen, TemplateSummary } from './components';

import { useWizardState } from './hooks';
import { NEON_TEMPLATE_BASE_PATH } from './constants';
import { GrowthExperimentsClient } from '../GrowthExperiments';
import { useFamiliaritySurveyData, HasuraFamiliaritySurvey } from '../Surveys';

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

  // dialog cannot be reopened once closed
  const { state, setState } = useWizardState(
    growthExperimentsClient,
    hasNeonAccess
  );

  const {
    showFamiliaritySurvey,
    data: familiaritySurveyData,
    onSkip: familiaritySurveyOnSkip,
    onOptionClick: familiaritySurveyOnOptionClick,
  } = useFamiliaritySurveyData();

  const templateBaseUrl = NEON_TEMPLATE_BASE_PATH;

  const transitionToTemplateSummary = () => {
    setState('template-summary');
  };

  const dismiss = () => {
    setState('hidden');
  };

  switch (state) {
    case 'landing-page': {
      return (
        // Radix dialog is being used for creating a layover component over the whole app.
        // It does not make sense to extend common dialog component to fit this one-off use case.
        //
        // modal={false} is set to prevent focus issues when multiple modals are visible,
        // for example survey modal and onboarding modal
        <Dialog.Root modal={false} open>
          <Dialog.Content className="fixed top-0 w-full h-full focus:outline-none bg-gray-50 overflow-hidden z-[100]">
            <TopHeaderBar />
            <div className="max-w-5xl p-md ml-auto mr-auto mt-xl">
              {showFamiliaritySurvey ? (
                <HasuraFamiliaritySurvey
                  data={familiaritySurveyData}
                  onSkip={familiaritySurveyOnSkip}
                  onOptionClick={familiaritySurveyOnOptionClick}
                />
              ) : (
                <ConnectDBScreen
                  dismissOnboarding={dismiss}
                  proceed={transitionToTemplateSummary}
                  hasNeonAccess={hasNeonAccess}
                  dispatch={dispatch}
                />
              )}
            </div>
          </Dialog.Content>
        </Dialog.Root>
      );
    }
    case 'template-summary': {
      return (
        <TemplateSummary
          templateUrl={templateBaseUrl}
          dismiss={dismiss}
          dispatch={dispatch}
        />
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
  if (!isCloudConsole(globals) && globals.userRole !== 'owner') {
    return null;
  }
  return <Root {...props} />;
}

export const RootWithoutCloudCheck = Root;
