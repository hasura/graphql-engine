import type {
  BulkMetadataQueryType,
  BulkQueryType,
  Metadata,
  SingleMetadataTypes,
} from '@hasura/shared/types';
import { BULK_METADATA_TYPES, BULK_QUERY_TYPES } from '@hasura/shared/types';
import { allowListHandlers } from './allowList';
import { queryCollectionHandlers } from './queryCollections';
import { openTelemetryHandlers } from './openTelemetry';
import { sourceHandlers } from './source';
import { restEndpointHandlers } from './rest';

/**
 * Error body shape returned by HGE for a metadata request that fails
 * validation. Domain handlers (`allowList.ts`, `queryCollections.ts`, ...)
 * return `{ status, error }` with this `error` payload.
 */
export type ResponseBodyMetadataTypeError = {
  code: string;
  error: string;
  path: string;
};

export type MetadataErrorResponse = {
  status: number;
  error: ResponseBodyMetadataTypeError;
};

/**
 * A single metadata action, e.g. `{ type: 'export_metadata', args: {} }` or
 * `{ type: 'create_query_collection', args: {...} }`.
 *
 * Kept structurally compatible with `TMigrationQuery` from `@hasura/metadata/api`
 * without importing it, so `shared/testing` does not depend on `metadata/api`
 * (that would create a project cycle: `metadata/api` tests import this package).
 */
export type SingleMetadataAction = {
  type: SingleMetadataTypes | 'export_metadata' | (string & {});
  args?: Record<string, any>;
  resource_version?: number;
};

export type BulkMetadataAction = {
  type: BulkMetadataQueryType | BulkQueryType;
  args: MetadataAction[];
  resource_version?: number;
};

export type MetadataAction = SingleMetadataAction | BulkMetadataAction;

export type MetadataReducer = (
  state: Metadata,
  action: MetadataAction,
) => Metadata | MetadataErrorResponse;

export const isMetadataError = (
  response: Metadata | MetadataErrorResponse,
): response is MetadataErrorResponse =>
  'error' in response && 'status' in response;

const BULK_TYPES = new Set<string>([
  ...BULK_METADATA_TYPES,
  ...BULK_QUERY_TYPES,
]);

const metadataHandlers: Record<string, MetadataReducer> = {
  // `export_metadata` simply reads the current state back out.
  export_metadata: (state) => state,
  ...allowListHandlers,
  ...queryCollectionHandlers,
  ...openTelemetryHandlers,
  ...sourceHandlers,
  ...restEndpointHandlers,
};

/**
 * Applies a metadata action to the in-memory metadata state, mirroring what a
 * real HGE server would do to the metadata document (but not the
 * `resource_version` bump — that is owned by the HTTP layer in `metadata.ts`).
 *
 * - `bulk` / `concurrent_bulk` / `bulk_atomic` / `bulk_keep_going`: each nested
 *   arg runs through the same reducer, threading the updated state. The first
 *   error short-circuits and is returned (mocks do not model
 *   `bulk_keep_going`'s continue-on-error semantics — see AGENTS.md).
 * - A known domain type runs its handler (which may return an error).
 * - An unknown type succeeds and leaves the metadata document unchanged, so the
 *   handlers stay reusable for hooks whose action type has no domain handler
 *   yet (HGE-like `{ message: 'success' }`, version still bumps in the HTTP
 *   layer).
 */
export const metadataReducer: MetadataReducer = (state, action) => {
  if (BULK_TYPES.has(action.type)) {
    const args = (action as BulkMetadataAction).args ?? [];
    let firstError: MetadataErrorResponse | undefined;
    const finalState = args.reduce<Metadata>((currentState, arg) => {
      if (firstError) {
        return currentState;
      }
      const response = metadataReducer(currentState, arg);
      if (isMetadataError(response)) {
        firstError = response;
        return currentState;
      }
      return response;
    }, state);

    return firstError ?? finalState;
  }

  const handler = metadataHandlers[action.type];
  if (handler) {
    return handler(state, action);
  }

  // Unknown metadata type: succeed and leave the document unchanged.
  return state;
};
