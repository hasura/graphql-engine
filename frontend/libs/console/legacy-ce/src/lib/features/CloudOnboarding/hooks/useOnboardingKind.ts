import * as React from 'react';
import { OnboardingKind, OnboardingResponseData } from '../types';
import { APIError } from '../../../hooks/error';
import { getLSItem, removeLSItem } from '@hasura/shared/utils';
import { skippedOnboardingThroughURLParamVariables } from '../constants';
import { emitOnboardingEvent } from '../utils';
import { oneClickDeploymentOnboardingKind } from '../OneClickDeployment/util';
import { OneClickDeploymentState } from '../OneClickDeployment';
import { LS_KEYS } from '@hasura/shared/types';

export const useOnboardingKind = (
  onboardingData: OnboardingResponseData | undefined,
  error: APIError | null,
  isLoading: boolean,
) => {
  const [kind, setKind] = React.useState<OnboardingKind>({ kind: 'none' });

  // `kind` is derived from the onboarding inputs but is also set imperatively by
  // `dismissOnboarding`, so it cannot be a plain `useMemo`. We recompute it
  // during render when the inputs change (React's "adjusting state while a prop
  // changes" pattern) instead of inside an effect, which would cause an extra
  // render pass. `prevInputs` starts as `null` so the initial inputs are
  // evaluated on mount, matching the original effect's first run.
  const [prevInputs, setPrevInputs] = React.useState<{
    onboardingData: OnboardingResponseData | undefined;
    error: APIError | null;
    isLoading: boolean;
  } | null>(null);

  if (
    !prevInputs ||
    prevInputs.onboardingData !== onboardingData ||
    prevInputs.error !== error ||
    prevInputs.isLoading !== isLoading
  ) {
    setPrevInputs({ onboardingData, error, isLoading });

    if (!(error || isLoading || !onboardingData?.data)) {
      const is_onboarded =
        onboardingData?.data?.user_onboarding[0]?.is_onboarded || false;
      const isSkipOnboardingFlagSet =
        getLSItem(LS_KEYS.skipOnboarding) === 'true';

      if (
        onboardingData?.data?.one_click_deployment[0] &&
        onboardingData?.data?.one_click_deployment[0]?.state !==
          OneClickDeploymentState.Completed
      ) {
        // TODO: Skip this block also if state == 'ERROR' && retryCount >= something
        setKind(oneClickDeploymentOnboardingKind(onboardingData));
      } else if (is_onboarded || isSkipOnboardingFlagSet) {
        setKind({ kind: 'none' });
      } else {
        // pass control to onboarding wizard
        setKind({ kind: 'use-case-onboarding' });
      }
    }
  }

  // Emitting the "skipped onboarding" analytics event and clearing the LS flag
  // are genuine side effects, so they stay in an effect.
  React.useEffect(() => {
    if (error || isLoading || !onboardingData?.data) {
      // TODO: emit onboarding error
      return;
    }

    if (getLSItem(LS_KEYS.skipOnboarding) === 'true') {
      emitOnboardingEvent(skippedOnboardingThroughURLParamVariables);
      removeLSItem(LS_KEYS.skipOnboarding);
    }
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
