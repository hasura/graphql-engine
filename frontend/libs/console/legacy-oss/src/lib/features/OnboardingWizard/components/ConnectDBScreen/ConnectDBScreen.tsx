import React from 'react';
import { useAppDispatch } from '@/store';
import { Button } from '@/new-components/Button';
import _push from '../../../../components/Services/Data/push';
import { OnboardingAnimation, OnboardingAnimationNavbar } from './components';

type ConnectDBScreenProps = {
  skipOnboarding: () => void;
  completeOnboarding: () => void;
};

export function ConnectDBScreen(props: ConnectDBScreenProps) {
  const { skipOnboarding, completeOnboarding } = props;
  const dispatch = useAppDispatch();

  const onClick = () => {
    // TODO: Due to routing being slow on prod, but wizard closing instantaneously, this causes
    // a flicker of `<Api />` tab before routing to `/data`.
    dispatch(_push(`/data/manage/connect`));
    completeOnboarding();
  };

  return (
    <div className="max-w-5xl p-md ml-auto mr-auto mt-xl">
      <h1 className="text-xl font-semibold text-cloud-darkest">
        Welcome to your new Hasura project
      </h1>
      <p>Let&apos;s get started by connecting your first database</p>

      <div className="mt-5">
        <OnboardingAnimationNavbar />
        <OnboardingAnimation />
      </div>

      <div className="flex items-center justify-between">
        <div className="cursor-pointer text-secondary text-sm hover:text-secondary-dark">
          <div data-trackid="onboarding-skip-button" onClick={skipOnboarding}>
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
      </div>
    </div>
  );
}
