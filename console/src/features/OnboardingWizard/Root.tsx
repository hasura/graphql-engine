import React, { useReducer } from 'react';
import * as Dialog from '@radix-ui/react-dialog';
import { TopHeaderBar, ConnectDBScreen } from './components';

export function Root() {
  // dialog cannot be reopened once closed
  const [isWizardOpen, closeWizard] = useReducer(() => false, true);

  // this dialog is being used to create a layover component over the whole app using react portal, and other handy functionalities radix dialog provides, which otherwise we'll have to implement manually
  // note that we have a common radix dialog, but that component has very specific styling, doesn't include react portal, and it did not make sense to extend that to fit this particular one-off use case
  return (
    <Dialog.Root open={isWizardOpen} onOpenChange={closeWizard}>
      <Dialog.Portal>
        <Dialog.Content className="fixed top-0 w-full h-full focus:outline-none bg-gray-50 overflow-hidden z-[101]">
          <TopHeaderBar />
          <ConnectDBScreen closeWizard={closeWizard} />
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog.Root>
  );
}
