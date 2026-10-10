import type { Metadata } from '@hasura/shared/types';
import { http, HttpResponse } from 'msw';
import { allowListInitialData } from './allowList';
import { queryCollectionInitialData } from './queryCollections';
import { openTelemetryInitialData } from './openTelemetry';
import { dataInitialData } from './source';
import { restEndpointsInitialData } from './rest';
import { MetadataAction, isMetadataError, metadataReducer } from './reducer';

export const createDefaultInitialData = (): Metadata => ({
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [],
    inherited_roles: [],
    ...allowListInitialData,
    ...queryCollectionInitialData,
    ...openTelemetryInitialData,
    ...dataInitialData,
    ...restEndpointsInitialData,
  },
});

/**
 * Subset of HGE's `/v1alpha1/config` response the mocks care about. Typed
 * loosely on purpose so callers can add whatever fields their hook reads
 * without `shared/testing` depending on `@hasura/metadata/api`'s `ServerConfig`.
 */
export type MockServerConfig = Record<string, unknown>;

const defaultConfig: MockServerConfig = {
  is_allow_list_enabled: true,
};

/** Body returned by a successful metadata write, mirroring HGE. */
export const METADATA_WRITE_SUCCESS = { message: 'success' } as const;

export type HandlersOptions = {
  /** Artificial latency (ms) applied to every response. */
  delay?: number;
  /** Initial metadata document (cloned so tests never share mutations). */
  initialData?: Metadata | (() => Metadata);
  /** Response for `GET /v1alpha1/config`. */
  config?: MockServerConfig;
  /**
   * When set, handlers match the absolute `${url}` + path (useful for
   * Storybook, which talks to a concrete origin). When omitted, handlers match
   * by path suffix (a wildcard prefix + `/v1/metadata`) so the same handlers
   * work for hooks hitting `http://localhost/v1/metadata` (server mode) and
   * Storybook hitting `http://localhost:8080/v1/metadata`.
   */
  url?: string;
};

const defaultOptions: Required<Pick<HandlersOptions, 'delay' | 'config'>> &
  Pick<HandlersOptions, 'initialData' | 'url'> = {
  delay: 0,
  config: defaultConfig,
  url: undefined,
  initialData: createDefaultInitialData,
};

const cloneInitialData = (
  initialData: HandlersOptions['initialData'],
): Metadata =>
  typeof initialData === 'function'
    ? initialData()
    : JSON.parse(JSON.stringify(initialData));

/**
 * Builds MSW v2 handlers backed by an isolated, in-memory Metadata document.
 *
 * Each call owns its own state clone, so a `setupServer(...handlers())` per
 * test file (or `server.use(...handlers())` per test) never leaks mutations
 * between tests.
 *
 * Endpoints:
 * - `GET  {url|*}/v1alpha1/config` -> `config`
 * - `POST {url|*}/v1/metadata`     -> runs the metadata reducer:
 *     - `export_metadata` returns the current document (no version bump)
 *     - a successful write updates state, bumps `resource_version`, and
 *       responds with `{ message: 'success' }` (not the whole document)
 *     - a reducer error responds with its `status` + error JSON (no bump)
 * - `GET  {url|*}/apis/metadata`   -> current document (CLI-mode export, called
 *     by `usePostMetadataMigration` when `consoleMode === 'cli'`)
 */
export const handlers = (options?: HandlersOptions) => {
  const { delay, initialData, config, url } = {
    ...defaultOptions,
    ...options,
  };

  let metadata = cloneInitialData(initialData);

  const path = (suffix: string) => (url ? `${url}${suffix}` : `*${suffix}`);

  const withDelay = async () => {
    if (delay) {
      await new Promise((resolve) => setTimeout(resolve, delay));
    }
  };

  return [
    http.get(path('/v1alpha1/config'), async () => {
      await withDelay();
      return HttpResponse.json(config, { status: 200 });
    }),

    // CLI mode: `usePostMetadataMigration` exports metadata from the CLI server
    // after a successful mutation (`GET .../apis/metadata?export=true`).
    http.get(path('/apis/metadata'), async () => {
      await withDelay();
      return HttpResponse.json(metadata, { status: 200 });
    }),

    http.post(path('/v1/metadata'), async ({ request }) => {
      await withDelay();
      const body = (await request.json()) as MetadataAction;

      const response = metadataReducer(metadata, body);

      if (isMetadataError(response)) {
        return HttpResponse.json(response.error, { status: response.status });
      }

      if (body.type === 'export_metadata') {
        return HttpResponse.json(metadata, { status: 200 });
      }

      // Successful write: commit the new document and bump the version, but
      // respond with the lightweight success body HGE returns for mutations.
      metadata = {
        ...response,
        resource_version: response.resource_version + 1,
      };

      return HttpResponse.json(METADATA_WRITE_SUCCESS, { status: 200 });
    }),
  ];
};
