import { allowListInitialData } from '@/features/AllowLists';
import { queryCollectionInitialData } from '@/features/QueryCollections';
import { TMigration } from '@/features/MetadataAPI';
import { Metadata } from '@/features/hasura-metadata-types';

import { ServerConfig } from '@/hooks';
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
  initialData: createDefaultInitialData,
  config: defaultConfig,
  url: 'http://localhost:8080',
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
    rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
      const body = req.body as TMigration['query'];

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
