import * as React from 'react';
import globals from '@/Globals';
import { isCloudConsole } from '@/utils/cloudConsole';
import { OneClickDeployment } from './OneClickDeployment';
import { OnboardingWizard } from './OnboardingWizard';
import { useOnboardingKind } from './hooks/useOnboardingKind';

export const CloudOnboardingWithoutCloudCheck = () => {
  const cloudOnboarding = useOnboardingKind();
  switch (cloudOnboarding.kind) {
    case 'wizard':
      return <OnboardingWizard />;
    case 'one-click-deployment':
      return (
        <OneClickDeployment
          deployment={cloudOnboarding.deployment}
          dismissOnboarding={cloudOnboarding.dismissOnboarding}
        />
      );
    default:
      return null;
  }
};

export const CloudOnboardingWithCloudCheck = () => {
  if (isCloudConsole(globals) && globals.userRole === 'owner') {
    return <CloudOnboardingWithoutCloudCheck />;
  }
  return null;
};

export const CloudOnboarding = CloudOnboardingWithCloudCheck;
