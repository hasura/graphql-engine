import * as React from 'react';
import globals from '@/Globals';
import { isCloudConsole } from '@/utils/cloudConsole';
import { OneClickDeployment } from './OneClickDeployment';
import { OnboardingWizard } from './OnboardingWizard';
import { useOnboardingKind } from './hooks/useOnboardingKind';
import { useFallbackApps } from './hooks/useFallbackApps';

export const CloudOnboardingWithoutCloudCheck = () => {
  const cloudOnboarding = useOnboardingKind();
  const fallbackApps = useFallbackApps(cloudOnboarding);
  switch (cloudOnboarding.kind) {
    case 'wizard':
      return <OnboardingWizard />;
    case 'one-click-deployment':
      return (
        <OneClickDeployment
          deployment={cloudOnboarding.deployment}
          dismissOnboarding={cloudOnboarding.dismissOnboarding}
          fallbackApps={fallbackApps}
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
