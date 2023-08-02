import { rest } from 'msw';
import { Metadata } from '../../../../../../../features/hasura-metadata-types';
import { ServerConfig } from '../../../../../../../hooks';

export const createDefaultInitialData = (): Metadata => ({
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'aPostgres',
        kind: 'postgres',
        tables: [
          {
            table: {
              name: 'Album',
              schema: 'public',
            },
          },
        ],
        functions: [
          {
            function: {
              name: 'search_album',
              schema: 'public',
            },
          },
        ],
        configuration: {
          connection_info: {
            database_url: 'postgres://postgres:pass@postgres:5432/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
      },
    ],
  },
});

type HandlersOptions = {
  delay?: number;
  initialData?: Metadata | (() => Metadata);
  config?: Partial<ServerConfig>;
  url?: string;
};

const defaultOptions: HandlersOptions = {
  delay: 0,
  config: {},
  url: 'http://localhost:8080',
  initialData: createDefaultInitialData,
};

export const handlers = (options?: HandlersOptions) => {
  const { url } = {
    ...defaultOptions,
    ...options,
  };

  return [
    rest.post(`${url}/v1/metadata`, async (req, res, ctx) => {
      const body = (await req.json()) as Record<string, any>;

      if (body.type === 'pg_set_function_customization') {
        return res(ctx.json({}));
      }

      const response = createDefaultInitialData();

      return res(ctx.json(response));
    }),
  ];
};
