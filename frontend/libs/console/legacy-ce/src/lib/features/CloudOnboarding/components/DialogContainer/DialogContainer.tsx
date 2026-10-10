import React, { ReactElement } from 'react';
import { TopHeaderBar } from '../TopHeaderBar/TopHeaderBar';
import {
  StepperNavbar,
  StepperNavbarStep,
} from '../StepperNavbar/StepperNavbar';
import { Dialog } from '@radix-ui/themes';

type DialogContainer = {
  children?: React.ReactNode;
  header: string | ReactElement<any>;
  subHeader?: string | ReactElement<any>;
  showSubHeaderAboveHeader?: boolean;
  showStepper?: boolean;
  stepperNavSteps?: StepperNavbarStep[];
  activeIndex?: number;
};

export const DialogContainer: React.FC<DialogContainer> = (props) => {
  const {
    activeIndex,
    showStepper,
    showSubHeaderAboveHeader,
    stepperNavSteps,
    header,
    subHeader,
  } = props;
  return (
    // Radix dialog is being used for creating a layover component over the whole app.
    // It does not make sense to extend common dialog component to fit this one-off use case.
    //
    // modal={false} is set to prevent focus issues when multiple modals are visible,
    // for example survey modal and onboarding modal
    <Dialog.Root open>
      <Dialog.Content className="w-full h-full focus:outline-none overflow-y-scroll">
        <TopHeaderBar />
        <div className="max-w-5xl py-4 ml-auto mr-auto">
          <div className="mb-5 font-sans">
            {showSubHeaderAboveHeader ? (
              <>
                {subHeader && (
                  <div className="text-sm text-muted-dark font-normal">
                    {subHeader}
                  </div>
                )}
                <h1 className="text-xl font-bold text-cloud-darkest">
                  {header}
                </h1>
              </>
            ) : (
              <>
                <h1 className="text-xl font-bold text-cloud-darkest">
                  {header}
                </h1>
                {subHeader && (
                  <div className="text-muted-dark font-normal">{subHeader}</div>
                )}
              </>
            )}
          </div>
          {showStepper && (
            <StepperNavbar
              steps={stepperNavSteps ?? []}
              activeIndex={activeIndex}
            />
          )}

          {props.children}
        </div>
      </Dialog.Content>
    </Dialog.Root>
  );
};
