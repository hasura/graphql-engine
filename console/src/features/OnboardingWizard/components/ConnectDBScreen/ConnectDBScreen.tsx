import React from 'react';
import { useAppDispatch } from '@/store';
import { Button } from '@/new-components/Button';
import _push from '../../../../components/Services/Data/push';
import { OnboardingAnimation, OnboardingAnimationNavbar } from './components';

type ConnectDBScreenProps = {
  closeWizard: () => void;
};

export function ConnectDBScreen(props: ConnectDBScreenProps) {
  const { closeWizard } = props;
  const dispatch = useAppDispatch();

  const onClick = () => {
    // we should change the route to `/data` first, and then close the wizard.
    // this is because routing is slow on prod, but wizard closes instantaneously.
    // if we close wizard first, it will show a flicker of `<Api />` tab before routing to `/data`.
    dispatch(_push(`/data/manage/connect`));
    closeWizard();
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
          <div data-trackid="onboarding-skip-button" onClick={closeWizard}>
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
