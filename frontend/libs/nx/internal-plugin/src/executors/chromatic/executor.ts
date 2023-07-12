import type { ExecutorContext } from '@nrwl/devkit';
import runCommands from 'nx/src/executors/run-commands/run-commands.impl';
import { FsTree } from 'nx/src/generators/tree';
import fetch from 'node-fetch';
import type { ChromaticExecutorSchema } from './schema';
import { postChromatic, preChromatic } from './chromatic';
import { executorPreFlightCheck } from './core/executorPreFlightCheck';

/**
 * Drop a message in every PR about the Chromatic status.
 * To avoid E2E testing the executor, please move all the logic inside functions that are easier to
 * unit test.
 */
export default async function runExecutor(
  options: ChromaticExecutorSchema,
  context: ExecutorContext
) {
  // PRE-FLIGHT CHECK
  const { projectName } = context;
  if (!context.workspace) {
    throw new Error('Worspace not found.');
  }

  const preFlightCheckResult = executorPreFlightCheck({
    envVars: {
      BUILDKITE: process.env.BUILDKITE,
      BUILDKITE_PULL_REQUEST: process.env.BUILDKITE_PULL_REQUEST,
    },
    executorContextProjectName: {
      projectName,
      projectConfiguration: context.workspace.projects[projectName ?? ''],
    },
  });

  const { distTarget } = preFlightCheckResult;

  // GITHUB APIS
  const { Octokit } = await import('octokit');
  const octokit = new Octokit({
    auth: process.env.GITHUB_TOKEN,
    request: {
      fetch: fetch,
    },
  });

  await preChromatic({
    octokit,
    preFlightCheckResult,
    envVars: {
      BUILDKITE_BUILD_URL: process.env.BUILDKITE_BUILD_URL,
      BUILDKITE_JOB_ID: process.env.BUILDKITE_JOB_ID,
    },
  });

  let uncaughtChromaticError: unknown;
  try {
    console.log('--- :chromatic: running chromatic');
    // RUN CHROMATIC
    await runCommands(
      {
        /*
        Flags summary:

        --exit-zero-on-changes
        When stories have changes, Chromatic fails the CI. --exit-zero-on-changes avoids failing the CI.
        We need it to control what happens in CI after Chromatic's build.


        --diagnostics
        Emits the diagnostic file that we parse to then post a comment on the PR.


        --force-rebuild
        By default, Chromatic optimizes re-running the same build without code changes. This happens when
        1. Chromatic goes green (no visual regressions) and we retry the Storybook job.
        2. Chromatic goes red (visual regressions/changes) and the changes are approved on Chromatic.
        The latter is the most common case. In this case
        - Chromatic will not run the build again if we re-run the CI job.
        - Not re-running means the diagnostic file is not emitted.
        - No diagnostic file means we cannot update the PR comment we previously left saying "there are changes in Chromatic"
        What we can do in the future is:
        - Creating a webhook that receives the Chromatic updates and then updates the PR comment and the CI job.
        - When Chromatic does not run, fetching the status from the Chromatic API and then updating the PR comment and the CI job.

        At the beginning, we opted to rebuild Chromatic every time.

        How can we identify a "skipped" build? At the time of writing, Chromatic emits something like this in CI

        > Skipping rebuild of an already fully passed/accepted build
        > A build for the same commit as the last build on the branch is considered a rebuild.
        > If the last build is passed or accepted, the rebuild is skipped because it shouldn't change anything.
        > You can override this using the --force-rebuild flag.

        And despite the Chromatic says "Wrote Chromatic diagnostics report to chromatic-diagnostics.json",
        in reality the diagnostic file is not emitted.
        */
        command: `npx chromatic --storybook-build-dir=${distTarget} --exit-zero-on-changes --diagnostics --force-rebuild`,
        __unparsed__: [],
      },
      context
    );
  } catch (error: unknown) {
    uncaughtChromaticError = error;
  }

  console.log('--- :chromatic: post-processing chromatic result');
  // POST CHROMATIC
  const result = await postChromatic({
    uncaughtChromaticError,
    tree: new FsTree(context.root, true),
    octokit,
    preFlightCheckResult,
    envVars: {
      BUILDKITE_BUILD_URL: process.env.BUILDKITE_BUILD_URL,
      BUILDKITE_JOB_ID: process.env.BUILDKITE_JOB_ID,
    },
  });

  console.log('--- :chromatic: post-processing chromatic result succeeded');

  return result;
}
