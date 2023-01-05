import * as React from 'react';
import { GitRepoDetails, OneClickDeploymentState } from '../OneClickDeployment';
import {
  useOnboardingData,
  emitOnboardingEvent,
  oneClickDeploymentOnboardingShown,
} from '../OnboardingWizard';

type OnboardingKind =
  | {
      kind: 'wizard';
    }
  | {
      kind: 'one-click-deployment';
      deployment: {
        deploymentId: number;
        gitRepoDetails: GitRepoDetails;
      };
    }
  | {
      kind: 'none';
    };

export const useOnboardingKind = () => {
  const { data, error, isLoading } = useOnboardingData();
  const [kind, setKind] = React.useState<OnboardingKind>({ kind: 'none' });

  React.useEffect(() => {
    if (error || isLoading) {
      setKind({ kind: 'none' });
      // TODO: emit onboarding error
      return;
    }

    if (!data?.data) {
      setKind({ kind: 'none' });
      // TODO: emit onboarding error
      return;
    }

    // if there's a pending one-click-deployment for this project,
    // show the deployment status
    // if the deployment is complete, show nothing
    const deployment = data.data.one_click_deployment[0];
    if (deployment?.state === OneClickDeploymentState.Completed) {
      setKind({ kind: 'none' });
      return;
    }
    if (deployment) {
      // TODO: Skip this block also if state == 'ERROR' && retryCount >= something

      setKind({
        kind: 'one-click-deployment',
        deployment: {
          deploymentId: data.data.one_click_deployment[0].id,
          gitRepoDetails: {
            url: data.data.one_click_deployment[0].git_repository_url,
            branch: data.data.one_click_deployment[0].git_repository_branch,
            hasuraDirectory: data.data.one_click_deployment[0].hasura_directory,
          },
        },
      });
      // emit onboarding event denoting onboarding through one click deployment
      emitOnboardingEvent(oneClickDeploymentOnboardingShown);
      return;
    }

    // pass control to onboarding wizard
    setKind({ kind: 'wizard' });
  }, [data, error, isLoading]);

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
