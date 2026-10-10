const maxAllowedLength = 255;
const unixEpochLength = 14;

export const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time
export const METADATA_QUERY_KEY = 'HASURA_METADATA';
export const INCONSISTENT_METADATA_QUERY_KEY = 'INCONSISTENT_METADATA';
export const MIGRATION_STATUS_QUERY_KEY = 'MIGRATION_STATUS';

export const MAX_ALLOWED_MIGRATION_LENGTH = maxAllowedLength - unixEpochLength;

export const defaultQueryOptions = {
  refetchOnWindowFocus: false,
  staleTime: DEFAULT_STALE_TIME,
};
