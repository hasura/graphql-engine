/* eslint-disable no-continue */

import {
  OneClickDeploymentStateTransition,
  CliLog,
  OneClickDeploymentState,
  UserFacingStep,
  ProgressState,
  ProgressStateStatus,
  GitRepoDetails,
} from './types';
import { OnboardingKind, OnboardingResponseData } from '../types';

// returns the error status if the step had an error in the latest workflow
// returns null if the step hasn't had an error in the latest workflow
const getStepError = (
  allStateTransitionsOrdered: OneClickDeploymentStateTransition[],
  step: UserFacingStep
): ProgressStateStatus | null => {
  let errorStatus: ProgressStateStatus | null = null;
  // run through all transitions to get the latest state of a step
  allStateTransitionsOrdered.forEach(transition => {
    if (transition.from_state === step) {
      if (transition.to_state === OneClickDeploymentState.Error) {
        errorStatus = {
          kind: 'error',
          error: transition.additional_info,
          logId: transition.id,
        };
      } else {
        errorStatus = null;
      }
    }
  });

  return errorStatus;
};

/**
 * returns success status if:
 * The step has never had an error after succeeding at any point
 * returns null if the step has never succeeded or has had an error
 */
const getStepSuccess = (
  allStateTransitionsOrdered: OneClickDeploymentStateTransition[],
  step: UserFacingStep
): ProgressStateStatus | null => {
  let successStatus: ProgressStateStatus | null = null;
  // run through all transitions to get the latest state of a step
  allStateTransitionsOrdered.forEach(transition => {
    if (transition.from_state === step) {
      // it's not a success if the step goes into an error
      if (transition.to_state === OneClickDeploymentState.Error) {
        successStatus = null;
        return;
      }

      // if it transitions to initialized state
      // and the step is not awaiting-env, then it was an error (possibly a timeout)
      if (transition.to_state === OneClickDeploymentState.Initialized) {
        successStatus = null;
        return;
      }

      successStatus = {
        kind: 'success',
      };
    }
  });

  // if a to_state of any transition is "completed", "completed" step is success
  if (
    step === OneClickDeploymentState.Completed &&
    allStateTransitionsOrdered.some(
      t => t.to_state === OneClickDeploymentState.Completed
    )
  ) {
    return {
      kind: 'success',
    };
  }

  return successStatus;
};

/**
 * Returns in-progress if the step is currently in progress
 * Returns idle if the step hasn't started in the current workflow
 * Returns awaiting if the step exited awaiting user input
 * Returns null if it's none of the above
 */
const getStepPendingState = (
  allStateTransitionsOrdered: OneClickDeploymentStateTransition[],
  step: UserFacingStep
): ProgressStateStatus | null => {
  // get the last transition
  const lastTransition =
    allStateTransitionsOrdered[allStateTransitionsOrdered.length - 1];

  // it can be awaiting only if the last step is awaiting
  if (step === OneClickDeploymentState.AwaitingEnvironmentVariables) {
    if (lastTransition && lastTransition?.to_state === step) {
      return {
        kind: 'awaiting',
        payload: lastTransition.additional_info,
      };
    }
  }

  // a step is `idle` if the step has never occured after the last initialized
  const lastInitializedIndex = allStateTransitionsOrdered
    .map(t => t.from_state)
    .lastIndexOf(OneClickDeploymentState.Initialized);
  if (lastInitializedIndex === -1) {
    return { kind: 'idle' };
  }

  // a step is in-progress if it's the to_state of the last transition
  // and it's not an exit
  if (
    lastTransition?.to_state === step &&
    ![
      OneClickDeploymentState.Completed,
      OneClickDeploymentState.AwaitingEnvironmentVariables,
    ].includes(step)
  ) {
    return { kind: 'in-progress' };
  }

  return null;
};

const getStepProgressState = (
  allStateTransitionsOrdered: OneClickDeploymentStateTransition[],
  step: UserFacingStep
): ProgressStateStatus => {
  // if there are no steps, only return in-progress for the initialized step
  if (allStateTransitionsOrdered.length === 0) {
    if (step === OneClickDeploymentState.Initialized) {
      return { kind: 'in-progress' };
    }
    return { kind: 'idle' };
  }

  // return success if success
  const stepSuccess = getStepSuccess(allStateTransitionsOrdered, step);
  if (stepSuccess) return stepSuccess;

  // return pending if in progress
  const stepPending = getStepPendingState(allStateTransitionsOrdered, step);
  if (stepPending) return stepPending;

  // return error if error
  const stepError = getStepError(allStateTransitionsOrdered, step);
  if (stepError) return stepError;

  return { kind: 'idle' };
};

export const getCliProgressState = (
  allStateTransitionsOrdered: OneClickDeploymentStateTransition[]
): ProgressState => {
  return {
    [OneClickDeploymentState.Initialized]: getStepProgressState(
      allStateTransitionsOrdered,
      OneClickDeploymentState.Initialized
    ),
    [OneClickDeploymentState.CloningGitRepository]: getStepProgressState(
      allStateTransitionsOrdered,
      OneClickDeploymentState.CloningGitRepository
    ),
    [OneClickDeploymentState.ReadingEnvironmentVariables]: getStepProgressState(
      allStateTransitionsOrdered,
      OneClickDeploymentState.ReadingEnvironmentVariables
    ),
    [OneClickDeploymentState.AwaitingEnvironmentVariables]:
      getStepProgressState(
        allStateTransitionsOrdered,
        OneClickDeploymentState.AwaitingEnvironmentVariables
      ),
    [OneClickDeploymentState.SufficientEnvironmentVariables]:
      getStepProgressState(
        allStateTransitionsOrdered,
        OneClickDeploymentState.SufficientEnvironmentVariables
      ),
    [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]:
      getStepProgressState(
        allStateTransitionsOrdered,
        OneClickDeploymentState.ApplyingMetadataMigrationsSeeds
      ),
    [OneClickDeploymentState.Completed]: getStepProgressState(
      allStateTransitionsOrdered,
      OneClickDeploymentState.Completed
    ),
  };
};

export const transformStatelogToCLILog = (
  allStateTransitions: OneClickDeploymentStateTransition[]
): CliLog[] => {
  let logs: CliLog[] = [];

  for (let i = 0; i < allStateTransitions.length; i++) {
    const currentTransition = allStateTransitions[i];
    const nextTransition = allStateTransitions[i + 1];

    /*
			if the workflow moved to `initialized` state,
			we clear all the previously transformed logs because
			we only want to show logs of the latest run of the workflow
		*/
    if (currentTransition?.to_state === OneClickDeploymentState.Initialized) {
      logs = [];
      continue;
    }

    // if the current transition is an error, push an error log
    if (currentTransition.to_state === OneClickDeploymentState.Error) {
      logs.push({
        kind: 'error',
        step: currentTransition.from_state,
        payload: currentTransition.additional_info,
      });
      continue;
    } else {
      logs.push({
        kind: 'success',
        step: currentTransition.from_state,
      });
    }

    // Resolve the to_state of the last log if there're no more logs
    if (!nextTransition) {
      if (
        currentTransition.to_state ===
        OneClickDeploymentState.AwaitingEnvironmentVariables
      ) {
        logs.push({
          kind: 'awaiting-env',
          step: currentTransition.to_state,
          payload: currentTransition.additional_info,
        });
      } else if (
        currentTransition.to_state === OneClickDeploymentState.Completed
      ) {
        logs.push({
          kind: 'success',
          step: currentTransition.to_state,
        });
      } else {
        logs.push({
          kind: 'loading',
          step: currentTransition.to_state,
        });
      }
    }
  }
  return logs;
};
export const shouldTriggerFirstDeployment = (
  allStateTransitionsOrdered: OneClickDeploymentStateTransition[]
): boolean => {
  return allStateTransitionsOrdered.length === 0;
};

export function getGitRepoFromUrl(gitRepoUrl: string) {
  let gitRepoPath = gitRepoUrl;
  try {
    const repoUrl = new URL(gitRepoUrl);
    const path = repoUrl?.pathname?.substring(1);
    if (path.length > 0) gitRepoPath = path;
  } catch (error) {
    console.warn(error);
  }
  return gitRepoPath;
}

export function getGitRepoFullLinkFromDetails(gitRepoDetails: GitRepoDetails) {
  const { url } = gitRepoDetails;
  const branch = gitRepoDetails.branch ?? 'main';
  const hasuraDirectory = gitRepoDetails.hasuraDirectory ?? 'hasura';
  return `${url}/tree/${branch}/${hasuraDirectory}`;
}

export function isHasuraPath(path: string) {
  const pathArray = path.split('/');
  return pathArray[0].trim() === 'hasura';
}

export const getSampleQueriesUrl = (gitRepoDetails: GitRepoDetails) => {
  try {
    const { url, branch, hasuraDirectory } = gitRepoDetails;
    const { pathname } = new URL(url);
    return `https://raw.githubusercontent.com${pathname}/${branch}/${hasuraDirectory}/sample-requests.graphql`;
  } catch {
    return '';
  }
};

export const oneClickDeploymentOnboardingKind = (
  onboardingData: OnboardingResponseData
): OnboardingKind => ({
  kind: 'one-click-deployment',
  deployment: {
    deploymentId: onboardingData.data.one_click_deployment[0].id,
    gitRepoDetails: {
      url: onboardingData.data.one_click_deployment[0].git_repository_url,
      branch: onboardingData.data.one_click_deployment[0].git_repository_branch,
      hasuraDirectory:
        onboardingData.data.one_click_deployment[0].hasura_directory,
    },
  },
  fallbackApps: onboardingData.data.one_click_deployment_sample_apps || [],
});
