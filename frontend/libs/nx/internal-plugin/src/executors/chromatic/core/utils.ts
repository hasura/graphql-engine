import type { Tree } from 'nx/src/generators/tree';
import { readJson } from '@nrwl/devkit';

// ------------------------------------------------------------

export async function readDiagnosticFile(tree: Tree, path: string) {
  let diagnosticFile: Record<string, unknown>;

  try {
    diagnosticFile = readJson(tree, path);
  } catch (error) {
    return undefined;
  }

  return diagnosticFile;
}

// ------------------------------------------------------------

export function getPullRequestNumber(
  // BUILDKITE_PULL_REQUEST should be a number but I'm not 100% sure...
  // see: https://buildkite.com/docs/pipelines/environment-variables#BUILDKITE_PULL_REQUEST
  buildkitePullRequest: unknown
) {
  if (!buildkitePullRequest) return;

  if (typeof buildkitePullRequest === 'number') return buildkitePullRequest;

  if (typeof buildkitePullRequest === 'string') {
    if (buildkitePullRequest === 'false') return;

    const prNumber = parseInt(buildkitePullRequest);
    if (isNaN(prNumber)) return;

    return prNumber;
  }

  return;
}
