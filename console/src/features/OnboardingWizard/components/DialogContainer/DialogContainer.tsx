import React from 'react';
import * as Dialog from '@radix-ui/react-dialog';
import { TopHeaderBar } from '../TopHeaderBar/TopHeaderBar';
import { StepperNavbar } from '../StepperNavbar/StepperNavbar';

type DialogContainer = {
  header: string;
  subHeader?: string;
  showStepper?: boolean;
  activeIndex?: number;
};

export const DialogContainer: React.FC<DialogContainer> = props => {
  const { activeIndex, showStepper, header, subHeader } = props;
  return (
    // Radix dialog is being used for creating a layover component over the whole app.
    // It does not make sense to extend common dialog component to fit this one-off use case.
    //
    // modal={false} is set to prevent focus issues when multiple modals are visible,
    // for example survey modal and onboarding modal
    <Dialog.Root modal={false} open>
      <Dialog.Content className="fixed top-0 w-full h-full focus:outline-none bg-gray-50 overflow-y-scroll z-[100]">
        <TopHeaderBar />
        <div className="max-w-5xl p-md ml-auto mr-auto mt-xl">
          <div className="mb-5">
            <h1 className="text-xl font-semibold text-cloud-darkest">
              {header}
            </h1>
            {subHeader && <p>{subHeader}</p>}
          </div>
          {showStepper && <StepperNavbar activeIndex={activeIndex} />}

          {props.children}
        </div>
      </Dialog.Content>
    </Dialog.Root>
  );
};
