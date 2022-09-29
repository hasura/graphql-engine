import React from 'react';
import * as Dialog from '@radix-ui/react-dialog';
import globals from '@/Globals';
import { isCloudConsole } from '@/utils/cloudConsole';
import { TopHeaderBar, ConnectDBScreen } from './components';
import { useWizardState } from './hooks';
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

  // dialog cannot be reopened once closed
  const { isWizardOpen, skipOnboarding, completeOnboarding } = useWizardState(
    growthExperimentsClient
  );

  const {
    showFamiliaritySurvey,
    data: familiaritySurveyData,
    onSkip: familiaritySurveyOnSkip,
    onOptionClick: familiaritySurveyOnOptionClick,
  } = useFamiliaritySurveyData();

  if (!isWizardOpen) return null;

  // Radix dialog is being used for creating a layover component over the whole app.
  // It does not make sense to extend common dialog component to fit this one-off use case.
  //
  // modal={false} is set to prevent focus issues when multiple modals are visible,
  // for example survey modal and onboarding modal
  return (
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
              skipOnboarding={skipOnboarding}
              completeOnboarding={completeOnboarding}
            />
          )}
        </div>
      </Dialog.Content>
    </Dialog.Root>
  );
}

export function RootWithCloudCheck(props: Props) {
  /*
   * Don't render Root component if current context is not cloud-console
   * and current user is not project owner
   */
  if (!isCloudConsole(globals)) {
    return null;
  }
  return <Root {...props} />;
}

export const RootWithoutCloudCheck = Root;
