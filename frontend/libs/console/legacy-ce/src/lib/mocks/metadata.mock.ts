import type { ServerConfig } from '../hooks';
import type { TMigration } from '../features/MetadataAPI';
import type { Metadata } from '../features/hasura-metadata-types';

import { allowListInitialData } from '../features/AllowLists';
import { queryCollectionInitialData } from '../features/QueryCollections';
import { openTelemetryInitialData } from '../features/OpenTelemetry';
import { restEndpointsInitialData } from '../features/RestEndpoints';
import { dataInitialData } from '../features/Data';

import { rest } from 'msw';
import { metadataReducer } from './actions';

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

const defaultConfig: Partial<ServerConfig> = {
  is_allow_list_enabled: true,
};

type HandlersOptions = {
  delay?: number;
  initialData?: Metadata | (() => Metadata);
  config?: Partial<ServerConfig>;
  url?: string;
};

const defaultOptions: HandlersOptions = {
  delay: 0,
  config: defaultConfig,
  url: 'http://localhost:8080',
  initialData: createDefaultInitialData,
};

export const handlers = (options?: HandlersOptions) => {
  const { delay, initialData, config, url } = {
    ...defaultOptions,
    ...options,
  };
  let metadata =
    typeof initialData === 'function'
      ? initialData()
      : JSON.parse(JSON.stringify(initialData));
  return [
    rest.get(`${url}/v1alpha1/config`, (req, res, ctx) => {
      return res(ctx.delay(delay), ctx.status(200), ctx.json(config));
    }),

    rest.post(`${url}/v1/metadata`, async (req, res, ctx) => {
      const body = (await req.json()) as TMigration['query'];

      const response = metadataReducer(metadata, body);

      if ('error' in response && response.error) {
        return res(
          ctx.delay(delay),
          ctx.status(response.status),
          ctx.json(response.error)
        );
      }

      if ('metadata' in response) {
        metadata = response;
      }

      return res(ctx.delay(delay), ctx.json(response));
    }),
  ];
};
