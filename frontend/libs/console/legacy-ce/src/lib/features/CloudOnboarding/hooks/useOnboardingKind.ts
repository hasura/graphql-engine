import * as React from 'react';
import { OnboardingKind, OnboardingResponseData } from '../types';
import { APIError } from '../../../hooks/error';
import { getLSItem, LS_KEYS, removeLSItem } from '../../../utils';
import { skippedOnboardingThroughURLParamVariables } from '../constants';
import { emitOnboardingEvent } from '../utils';
import { oneClickDeploymentOnboardingKind } from '../OneClickDeployment/util';
import { OneClickDeploymentState } from '../OneClickDeployment';

export const useOnboardingKind = (
  onboardingData: OnboardingResponseData | undefined,
  error: APIError | null,
  isLoading: boolean
) => {
  const [kind, setKind] = React.useState<OnboardingKind>({ kind: 'none' });

  React.useEffect(() => {
    if (error || isLoading || !onboardingData?.data) {
      // TODO: emit onboarding error
      return;
    }

    const is_onboarded =
      onboardingData?.data?.user_onboarding[0]?.is_onboarded || false;
    const isSkipOnboardingFlagSet =
      getLSItem(LS_KEYS.skipOnboarding) === 'true';

    if (isSkipOnboardingFlagSet) {
      emitOnboardingEvent(skippedOnboardingThroughURLParamVariables);
      removeLSItem(LS_KEYS.skipOnboarding);
    }

    if (
      onboardingData?.data?.one_click_deployment[0] &&
      onboardingData?.data?.one_click_deployment[0]?.state !==
        OneClickDeploymentState.Completed
    ) {
      // TODO: Skip this block also if state == 'ERROR' && retryCount >= something
      setKind(oneClickDeploymentOnboardingKind(onboardingData));
      return;
    }

    if (is_onboarded || isSkipOnboardingFlagSet) {
      setKind({ kind: 'none' });
      return;
    }

    // pass control to onboarding wizard
    setKind({ kind: 'use-case-onboarding' });
  }, [onboardingData, error, isLoading]);

  // if there's error fetching onboarding data
  // do not show anything and proceed to the console

  return {
    ...kind,
    dismissOnboarding: () => {
      setKind({
        kind: 'none',
      });
    },
  };
};
