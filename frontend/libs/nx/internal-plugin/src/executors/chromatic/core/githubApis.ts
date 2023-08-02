import type { Octokit } from 'octokit';
import { CHROMATIC_COMMENT_NEEDLE } from './generateCommentStrategy';

export async function getExistingPrComment(octokit: Octokit, prNumber: number) {
  const prComments = await octokit.rest.issues.listComments({
    owner: 'hasura',
    repo: 'graphql-engine-mono',
    issue_number: prNumber,
  });

  return prComments.data.find(comment =>
    // it takes for granted only one comment exists
    comment.body?.includes(CHROMATIC_COMMENT_NEEDLE)
  );
}

export async function addNewPrComment(
  octokit: Octokit,
  prNumber: number,
  comment: string
) {
  return await octokit.rest.issues.createComment({
    owner: 'hasura',
    repo: 'graphql-engine-mono',
    issue_number: prNumber,
    body: comment,
  });
}

export async function updatePrComment(
  octokit: Octokit,
  comment: string,
  commentId: number
) {
  return await octokit.rest.issues.updateComment({
    owner: 'hasura',
    repo: 'graphql-engine-mono',
    comment_id: commentId,
    body: comment,
  });
}
