import { Metadata } from '../../hasura-metadata-types';
import { rest } from 'msw';

const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [],
    backend_configs: {
      dataconnector: {
        sqlite: {
          uri: 'http://host.docker.internal:8100',
        },
        csv: {
          uri: 'http://host.docker.internal:8101',
        },
      },
    },
  },
};

export const handlers = () => [
  rest.post(`http://localhost:8080/v1/metadata`, (req, res, ctx) => {
    const requestBody = req.body as Record<string, any>;
    if (requestBody.type === 'export_metadata') return res(ctx.json(metadata));

    if (requestBody.type === 'dc_delete_agent') {
      const agentName = requestBody.args.name;
      delete metadata.metadata.backend_configs?.dataconnector[agentName];
      return res(ctx.json(metadata));
    }

    if (requestBody.type === 'dc_add_agent') {
      const { name, url: uri } = requestBody.args;
      metadata.metadata = {
        ...metadata.metadata,
        backend_configs: {
          ...metadata.metadata.backend_configs,
          dataconnector: {
            ...metadata.metadata.backend_configs?.dataconnector,
            [name]: {
              uri,
            },
          },
        },
      };
      return res(ctx.json({ message: 'success' }));
    }

    return res(ctx.json(metadata));
  }),
];
