import { rest } from 'msw';
import config from './config';
import metadata from './metadata';
import save from './save';
import deleteMocks from './delete';

export const handlers = () => [
  rest.get('http://localhost:8080/v1alpha1/config', async (req, res, ctx) => {
    return res(ctx.status(200), ctx.json(config));
  }),
  rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
    const reqBody = await req.json<{
      type: string;
      args: any;
    }>();
    if (reqBody.type === 'export_metadata') {
      return res(ctx.status(200), ctx.json({ metadata }));
    }
    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 2 &&
      reqBody.args[0].type === 'pg_drop_logical_model_select_permission' &&
      reqBody.args[1].type === 'pg_create_logical_model_select_permission'
    ) {
      return res(ctx.status(200), ctx.json(save.response));
    }
    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 1 &&
      reqBody.args[0].type === 'pg_drop_logical_model_select_permission'
    ) {
      return res(ctx.status(200), ctx.json(deleteMocks.response));
    }
  }),
  rest.get('http://localhost:8080/v1/entitlement', async (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({
        metadata_db_id: '58a9e616-5fe9-4277-95fa-e27f9d45e177',
        status: 'none',
      })
    );
  }),
];

export const deleteHandlers = () => {
  let hasDeleted = false;
  return [
    rest.get('http://localhost:8080/v1alpha1/config', async (req, res, ctx) => {
      return res(ctx.status(200), ctx.json(config));
    }),
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const reqBody = await req.json<{
        type: string;
        args: any;
      }>();
      if (reqBody.type === 'export_metadata' && hasDeleted) {
        return res(
          ctx.status(200),
          ctx.json({
            metadata: {
              ...metadata,
              sources: [
                ...metadata.sources.map(source => {
                  if (source.name !== 'Postgres') {
                    return source;
                  }
                  return {
                    ...source,
                    logical_models: source.logical_models.map(logical_model => {
                      if (logical_model.name !== 'LogicalModel') {
                        return logical_model;
                      }
                      return {
                        fields: logical_model.fields,
                        name: logical_model.name,
                        // Omit select_permissions to simulate deletion
                      };
                    }),
                  };
                }),
              ],
            },
          })
        );
      }
      if (reqBody.type === 'export_metadata') {
        return res(
          ctx.status(200),
          ctx.json({
            metadata,
          })
        );
      }
      if (reqBody.type === 'export_metadata') {
        return res(ctx.status(200), ctx.json({ metadata }));
      }
      if (
        reqBody.type === 'bulk' &&
        reqBody.args.length === 2 &&
        reqBody.args[0].type === 'pg_drop_logical_model_select_permission' &&
        reqBody.args[1].type === 'pg_create_logical_model_select_permission'
      ) {
        return res(ctx.status(200), ctx.json(save.response));
      }
      if (
        reqBody.type === 'bulk' &&
        reqBody.args.length === 1 &&
        reqBody.args[0].type === 'pg_drop_logical_model_select_permission'
      ) {
        hasDeleted = true;
        return res(ctx.status(200), ctx.json(deleteMocks.response));
      }
    }),
    rest.get('http://localhost:8080/v1/entitlement', async (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.json({
          metadata_db_id: '58a9e616-5fe9-4277-95fa-e27f9d45e177',
          status: 'none',
        })
      );
    }),
  ];
};
