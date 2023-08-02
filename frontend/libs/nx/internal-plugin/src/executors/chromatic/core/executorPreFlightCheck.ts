import type { ProjectConfiguration } from '@nrwl/devkit';

import { getPullRequestNumber } from './utils';

export type PreFlightCheckParams = {
  executorContextProjectName: {
    projectName: string | undefined;
    projectConfiguration: ProjectConfiguration | undefined;
  };
  envVars: {
    BUILDKITE: string | undefined;
    BUILDKITE_PULL_REQUEST: string | undefined;
  };
};

export type PreFlightCheckResult =
  | {
      mode: 'mainBranch';
      distTarget: string;
    }
  | {
      mode: 'pr';
      distTarget: string;
      prNumber: number;
    };

/**
 * Check if the executor must run or not. It the executor must run, the dist target directory is returned.
 */
export function executorPreFlightCheck(
  params: PreFlightCheckParams
): PreFlightCheckResult {
  const {
    executorContextProjectName: { projectName, projectConfiguration },
    envVars: { BUILDKITE, BUILDKITE_PULL_REQUEST },
  } = params;

  // CHECK IF WE ARE IN CI
  if (!BUILDKITE) {
    throw new Error('Chromatic executor should only been run in CI');
  }

  const distTarget =
    projectConfiguration?.targets?.['build-storybook']?.options?.outputDir ??
    `dist/storybook/console/${projectName}`;

  // CHECK PROJECT NAME
  if (!projectName) {
    throw new Error(`Unexpected project name ${projectName}`);
  }

  // CHECK IF WE ARE ON A PR
  const prNumber = getPullRequestNumber(BUILDKITE_PULL_REQUEST);
  if (typeof prNumber === 'number') {
    return {
      mode: 'pr',
      prNumber,
      distTarget,
    };
  } else {
    console.log(`Comments are only added to PRs`);
    return {
      mode: 'mainBranch',
      distTarget,
    };
  }
}
