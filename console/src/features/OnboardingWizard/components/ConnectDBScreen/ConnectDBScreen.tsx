import React from 'react';
import { useAppDispatch } from '@/store';
import { Button } from '@/new-components/Button';
import Globals from '@/Globals';
import { hasLuxFeatureAccess } from '@/utils/cloudConsole';
import { OnboardingAnimation, OnboardingAnimationNavbar } from './components';
import { NeonOnboarding } from './NeonOnboarding';
import _push from '../../../../components/Services/Data/push';

type ConnectDBScreenProps = {
  skipOnboarding: () => void;
  completeOnboarding: () => void;
};

export function ConnectDBScreen(props: ConnectDBScreenProps) {
  const { skipOnboarding, completeOnboarding } = props;
  const dispatch = useAppDispatch();

  const onError = (error?: string) => {
    if (error) {
      throw new Error(error);
    }
  };

  const onClick = () => {
    // TODO: Due to routing being slow on prod, but wizard closing instantaneously, this causes
    // a flicker of `<Api />` tab before routing to `/data`.
    dispatch(_push(`/data/manage/connect`));
    completeOnboarding();
  };

  return (
    <>
      <h1 className="text-xl font-semibold text-cloud-darkest">
        Welcome to your new Hasura project!
      </h1>
      <p>Let&apos;s get started by connecting your first database</p>

      <div className="mt-5">
        <OnboardingAnimationNavbar />
        <OnboardingAnimation />
      </div>

      <div className="flex items-center justify-between">
        {hasLuxFeatureAccess(Globals, 'NeonDatabaseIntegration') ? (
          <NeonOnboarding
            dispatch={dispatch}
            onSkip={skipOnboarding}
            onCompletion={completeOnboarding}
            onError={onError}
          />
        ) : (
          <>
            <div className="cursor-pointer text-secondary text-sm hover:text-secondary-dark">
              <div
                data-trackid="onboarding-skip-button"
                onClick={skipOnboarding}
              >
                Skip setup, continue to dashboard
              </div>
            </div>
            <Button
              data-trackid="onboarding-connect-db-button"
              mode="primary"
              onClick={onClick}
            >
              Connect Your Database
            </Button>
          </>
        )}
      </div>
    </>
  );
}
