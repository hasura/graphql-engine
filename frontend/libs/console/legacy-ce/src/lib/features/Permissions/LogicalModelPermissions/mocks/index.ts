import { http, HttpResponse } from 'msw';
import config from './config';
import metadata from './metadata';
import save from './save';
import deleteMocks from './delete';

export const handlers = () => [
  http.get('http://localhost:8080/v1alpha1/config', async () => {
    return HttpResponse.json(config, { status: 200 });
  }),
  http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
    const reqBody = (await request.json()) as {
      type: string;
      args: any;
    };
    if (reqBody.type === 'export_metadata') {
      return HttpResponse.json({ metadata }, { status: 200 });
    }
    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 2 &&
      reqBody.args[0].type === 'pg_drop_logical_model_select_permission' &&
      reqBody.args[1].type === 'pg_create_logical_model_select_permission'
    ) {
      return HttpResponse.json(save.response, { status: 200 });
    }
    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 1 &&
      reqBody.args[0].type === 'pg_drop_logical_model_select_permission'
    ) {
      return HttpResponse.json(deleteMocks.response, { status: 200 });
    }
  }),
  http.get('http://localhost:8080/v1/entitlement', async () => {
    return HttpResponse.json(
      {
        metadata_db_id: '58a9e616-5fe9-4277-95fa-e27f9d45e177',
        status: 'none',
      },
      { status: 200 },
    );
  }),
];

export const deleteHandlers = () => {
  let hasDeleted = false;
  return [
    http.get('http://localhost:8080/v1alpha1/config', async () => {
      return HttpResponse.json(config, { status: 200 });
    }),
    http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
      const reqBody = (await request.json()) as {
        type: string;
        args: any;
      };
      if (reqBody.type === 'export_metadata' && hasDeleted) {
        return HttpResponse.json(
          {
            metadata: {
              ...metadata,
              sources: [
                ...metadata.sources.map((source) => {
                  if (source.name !== 'Postgres') {
                    return source;
                  }
                  return {
                    ...source,
                    logical_models: source.logical_models.map(
                      (logical_model) => {
                        if (logical_model.name !== 'LogicalModel') {
                          return logical_model;
                        }
                        return {
                          fields: logical_model.fields,
                          name: logical_model.name,
                          // Omit select_permissions to simulate deletion
                        };
                      },
                    ),
                  };
                }),
              ],
            },
          },
          { status: 200 },
        );
      }
      if (reqBody.type === 'export_metadata') {
        return HttpResponse.json(
          {
            metadata,
          },
          { status: 200 },
        );
      }
      if (reqBody.type === 'export_metadata') {
        return HttpResponse.json({ metadata }, { status: 200 });
      }
      if (
        reqBody.type === 'bulk' &&
        reqBody.args.length === 2 &&
        reqBody.args[0].type === 'pg_drop_logical_model_select_permission' &&
        reqBody.args[1].type === 'pg_create_logical_model_select_permission'
      ) {
        return HttpResponse.json(save.response, { status: 200 });
      }
      if (
        reqBody.type === 'bulk' &&
        reqBody.args.length === 1 &&
        reqBody.args[0].type === 'pg_drop_logical_model_select_permission'
      ) {
        hasDeleted = true;
        return HttpResponse.json(deleteMocks.response, { status: 200 });
      }
    }),
    http.get('http://localhost:8080/v1/entitlement', async () => {
      return HttpResponse.json(
        {
          metadata_db_id: '58a9e616-5fe9-4277-95fa-e27f9d45e177',
          status: 'none',
        },
        { status: 200 },
      );
    }),
  ];
};
