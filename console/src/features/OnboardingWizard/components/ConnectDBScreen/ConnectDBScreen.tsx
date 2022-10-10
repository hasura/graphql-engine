import React from 'react';
import { Dispatch } from '@/types';
import { Button } from '@/new-components/Button';
import { OnboardingAnimation } from './components/OnboardingAnimation';
import { NeonOnboarding } from './components/NeonOnboarding';
import _push from '../../../../components/Services/Data/push';
import {
  persistSkippedOnboarding,
  persistOnboardingCompletion,
} from '../../utils';

type ConnectDBScreenProps = {
  proceed: VoidFunction;
  dismissOnboarding: VoidFunction;
  hasNeonAccess: boolean;
  dispatch: Dispatch;
  setStepperIndex: (index: number) => void;
};

export function ConnectDBScreen(props: ConnectDBScreenProps) {
  const {
    proceed,
    dismissOnboarding,
    hasNeonAccess,
    dispatch,
    setStepperIndex,
  } = props;

  const pushToConnectDBPage = () => {
    // TODO: Due to routing being slow on prod, but wizard closing instantaneously, this causes
    // a flicker of `<Api />` tab before routing to `/data`.
    dispatch(_push(`/data/manage/connect`));
    dismissOnboarding();
  };

  const onClickConnectDB = () => {
    persistOnboardingCompletion();
    pushToConnectDBPage();
  };

  const skipLandingPage = React.useCallback(() => {
    persistSkippedOnboarding();
    dismissOnboarding();
  }, [dismissOnboarding]);

  return (
    <>
      <OnboardingAnimation />

      <div className="flex items-center justify-between">
        {hasNeonAccess ? (
          <NeonOnboarding
            dispatch={dispatch}
            dismiss={dismissOnboarding}
            proceed={proceed}
            setStepperIndex={setStepperIndex}
          />
        ) : (
          <>
            <div className="cursor-pointer text-secondary text-sm hover:text-secondary-dark">
              <div
                data-trackid="onboarding-skip-button"
                onClick={skipLandingPage}
              >
                Skip setup, continue to dashboard
              </div>
            </div>
            <Button
              data-trackid="onboarding-connect-db-button"
              mode="primary"
              onClick={onClickConnectDB}
            >
              Connect Your Database
            </Button>
          </>
        )}
      </div>
    </>
  );
}
