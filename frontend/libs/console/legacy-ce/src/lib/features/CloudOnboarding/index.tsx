import { isCloudConsole } from '@hasura/shared/utils';
import { OneClickDeployment } from './OneClickDeployment';
import { NeonOnboarding, useOnboardingData } from './NeonOnboardingWizard';
import { UseCaseOnboarding } from './UseCaseOnboarding';
import { useOnboardingKind } from './hooks/useOnboardingKind';
import { useFallbackApps } from './hooks/useFallbackApps';
import { useAppContext } from '@hasura/shared/context';

export const CloudOnboardingWithoutCloudCheck = () => {
  const { data, error, isLoading } = useOnboardingData();
  const cloudOnboarding = useOnboardingKind(data, error, isLoading);
  const fallbackApps = useFallbackApps(cloudOnboarding);
  switch (cloudOnboarding.kind) {
    case 'neon-onboarding':
      return <NeonOnboarding onboardingData={data} />;
    case 'one-click-deployment':
      return (
        <OneClickDeployment
          deployment={cloudOnboarding.deployment}
          dismissOnboarding={cloudOnboarding.dismissOnboarding}
          fallbackApps={fallbackApps}
        />
      );
    case 'use-case-onboarding':
      return <UseCaseOnboarding dismiss={cloudOnboarding.dismissOnboarding} />;
    default:
      return null;
  }
};

export const CloudOnboardingWithCloudCheck = () => {
  const { envVars } = useAppContext();

  if (isCloudConsole(envVars) && envVars.userRole === 'owner') {
    return <CloudOnboardingWithoutCloudCheck />;
  }
  return null;
};

export const CloudOnboarding = CloudOnboardingWithCloudCheck;
