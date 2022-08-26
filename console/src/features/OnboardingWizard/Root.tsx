import React from 'react';
import * as Dialog from '@radix-ui/react-dialog';
import { TopHeaderBar, ConnectDBScreen } from './components';
import { useWizardState } from './hooks';
import { GrowthExperimentsClient } from '../GrowthExperiments';

type Props = {
  growthExperimentsClient: GrowthExperimentsClient;
};

/**
 * Parent container for the onboarding wizard. Takes care of assembling and rendering all steps.
 */
export function Root(props: Props) {
  const { growthExperimentsClient } = props;

  // dialog cannot be reopened once closed
  const {
    isWizardOpen,
    skipOnboarding,
    completeOnboarding,
    setIsWizardOpen,
  } = useWizardState(growthExperimentsClient);

  // Radix dialog is being used for creating a layover component over the whole app using react portal,
  // and for other handy functionalities radix-dialog provides, which otherwise we'll have to implement manually.
  // Not extending the common dialog, as it does not make sense to update common component to fit this one-off use case.
  return (
    <Dialog.Root open={isWizardOpen} onOpenChange={setIsWizardOpen}>
      <Dialog.Portal>
        <Dialog.Content className="fixed top-0 w-full h-full focus:outline-none bg-gray-50 overflow-hidden z-[101]">
          <TopHeaderBar />
          <ConnectDBScreen
            skipOnboarding={skipOnboarding}
            completeOnboarding={completeOnboarding}
          />
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog.Root>
  );
}
