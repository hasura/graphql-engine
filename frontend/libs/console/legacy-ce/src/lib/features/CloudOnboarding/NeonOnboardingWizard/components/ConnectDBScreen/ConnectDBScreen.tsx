import React, { useEffect } from 'react';
import { Dispatch } from '../../../../../types';

import { OnboardingAnimation } from './components/OnboardingAnimation';
import { NeonOnboarding } from './components/NeonOnboarding';
import { trackCustomEvent } from '../../../../Analytics';

type ConnectDBScreenProps = {
  proceed: VoidFunction;
  dismissOnboarding: VoidFunction;
  dispatch: Dispatch;
  setStepperIndex: (index: number) => void;
};

export function ConnectDBScreen(props: ConnectDBScreenProps) {
  useEffect(() => {
    trackCustomEvent({
      location: 'Console',
      action: 'Load',
      object: 'Neon Onboarding Wizard',
    });
  }, []);
  const { proceed, dismissOnboarding, dispatch, setStepperIndex } = props;

  return (
    <>
      <OnboardingAnimation />
      <div className="flex items-center justify-between">
        <NeonOnboarding
          dispatch={dispatch}
          dismiss={dismissOnboarding}
          proceed={proceed}
          setStepperIndex={setStepperIndex}
        />
      </div>
    </>
  );
}
