import type { Octokit } from 'octokit';

import { Tree } from 'nx/src/generators/tree';

import type { PreFlightCheckResult } from './core/executorPreFlightCheck';
import { readDiagnosticFile } from './core/utils';
import {
  CHROMATIC_COMMENT_NEEDLE,
  generateCommentStrategy,
} from './core/generateCommentStrategy';

import {
  addNewPrComment,
  updatePrComment,
  getExistingPrComment,
} from './core/githubApis';

type PreChromaticParams = {
  octokit: Octokit;
  preFlightCheckResult: PreFlightCheckResult;
  envVars: {
    BUILDKITE_BUILD_URL: string | undefined;
    BUILDKITE_JOB_ID: string | undefined;
  };
};

/**
 * Add/update a comment before Chromatic is launched.
 */
export async function preChromatic(options: PreChromaticParams) {
  const {
    octokit,
    preFlightCheckResult,
    envVars: { BUILDKITE_BUILD_URL, BUILDKITE_JOB_ID },
  } = options;

  console.log('preChromatic function');

  if (preFlightCheckResult.mode !== 'pr') {
    console.log('skipped because it is not a PR');
    return;
  }

  const { prNumber } = preFlightCheckResult;

  console.log('Read the existing PR comments');
  const existingComment = await getExistingPrComment(octokit, prNumber);

  const newComment = `
  ### ⏳ Chromatic Visual Regression Report
  CI job is running, this comment will be updated once Chromatic is done.

  See the CI job [here](${BUILDKITE_BUILD_URL}#${BUILDKITE_JOB_ID}).

_Sent with 💌 from the frontenders of the Hasura Platform team_.

<!-- ${CHROMATIC_COMMENT_NEEDLE} (allows uniquely identifying it's a comment from this executor) -->
`;

  if (existingComment) {
    console.log(`Update the PR message (id: ${existingComment.id})`);
    await updatePrComment(octokit, newComment, existingComment.id);
  } else {
    console.log('Add the PR message');
    await addNewPrComment(octokit, prNumber, newComment);
  }
}

type PostChromaticParams = {
  tree: Tree;
  octokit: Octokit;
  uncaughtChromaticError: unknown;
  preFlightCheckResult: PreFlightCheckResult;

  envVars: {
    BUILDKITE_BUILD_URL: string | undefined;
    BUILDKITE_JOB_ID: string | undefined;
  };
};

/**
 * Add/update a comment reflecting the Chromatic status to the PR and make the CI fail if needed.
 */
export async function postChromatic(params: PostChromaticParams) {
  const {
    tree,
    octokit,
    preFlightCheckResult,
    uncaughtChromaticError,
    envVars: { BUILDKITE_JOB_ID, BUILDKITE_BUILD_URL },
  } = params;

  console.log('postChromatic function');

  // We only need to add/update the comment in PRs. Chromatic running on the main branch does not
  // need a comment (but needs the CI-related check)
  const shouldComment = preFlightCheckResult.mode === 'pr';

  console.log('Read the existing PR comments');
  const existingComment = shouldComment
    ? await getExistingPrComment(octokit, preFlightCheckResult.prNumber)
    : undefined;

  console.log('Read diagnostic file');
  const diagnosticFile = await readDiagnosticFile(
    tree,
    '/chromatic-diagnostics.json'
  );

  const commentStrategy = generateCommentStrategy({
    uncaughtChromaticError,
    diagnosticFile,
    envVars: {
      BUILDKITE_JOB_ID,
      BUILDKITE_BUILD_URL,
    },
  });
  console.log('Comment strategy');
  console.log(commentStrategy);

  if (shouldComment) {
    const { prNumber } = preFlightCheckResult;
    if (existingComment) {
      console.log(`Update the PR message (id: ${existingComment.id})`);
      await updatePrComment(
        octokit,
        commentStrategy.comment,
        existingComment.id
      );
    } else {
      console.log('Add the PR message');
      await addNewPrComment(octokit, prNumber, commentStrategy.comment);
    }
  }

  if (commentStrategy.error) {
    console.log('CI Failure');

    throw commentStrategy.error;
  }

  return { success: true };
}
