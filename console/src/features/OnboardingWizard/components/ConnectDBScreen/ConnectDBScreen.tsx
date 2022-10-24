import React from 'react';
import { Dispatch } from '@/types';
import { Button } from '@/new-components/Button';

import { Analytics } from '@/features/Analytics';

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
              <Analytics name="onboarding-skip-button">
                <div onClick={skipLandingPage}>
                  Skip setup, continue to dashboard
                </div>
              </Analytics>
            </div>
            <Analytics
              name="onboarding-connect-db-button"
              passHtmlAttributesToChildren
            >
              <Button mode="primary" onClick={onClickConnectDB}>
                Connect Your Database
              </Button>
            </Analytics>
          </>
        )}
      </div>
    </>
  );
}
