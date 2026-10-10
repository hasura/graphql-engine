import { Metadata } from '@hasura/shared/types';
import { http, HttpResponse } from 'msw';

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
  http.post(`http://localhost:8080/v1/metadata`, async ({ request }) => {
    const requestBody = (await request.json()) as Record<string, any>;
    if (requestBody.type === 'export_metadata')
      return HttpResponse.json(metadata);

    if (requestBody.type === 'dc_delete_agent') {
      const agentName = requestBody.args.name;
      delete metadata.metadata.backend_configs?.dataconnector[agentName];
      return HttpResponse.json(metadata);
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
      return HttpResponse.json({ message: 'success' });
    }

    return HttpResponse.json(metadata);
  }),
];
