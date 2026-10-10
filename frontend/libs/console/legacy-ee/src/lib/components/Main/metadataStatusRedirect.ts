/**
 * Pure decision for whether the EE console should redirect to the metadata
 * status page on load.
 *
 * It must only redirect once the inconsistent-metadata query has RESOLVED and
 * confirmed an actual inconsistency — never while the query is still pending
 * (data `undefined`), otherwise `!undefined?.is_consistent` redirects
 * unconditionally on first render. Access policy is respected via `hasDataAccess`.
 */
export type InconsistentMetadataLike =
  { is_consistent: boolean } | undefined | null;

export function shouldRedirectToMetadataStatus(args: {
  hasDataAccess: boolean;
  isLoading: boolean;
  inconsistentMetadata: InconsistentMetadataLike;
}): boolean {
  const { hasDataAccess, isLoading, inconsistentMetadata } = args;

  // Query not resolved yet (or no data) -> do not redirect.
  if (isLoading || !inconsistentMetadata) {
    return false;
  }

  // Only redirect for a confirmed inconsistency, and only for users with data access.
  return hasDataAccess && inconsistentMetadata.is_consistent === false;
}
