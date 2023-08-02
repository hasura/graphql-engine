import metadata from './metadata';
import { rest } from 'msw';
import config from './config';

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
      reqBody.args.length === 1 &&
      reqBody.args[0].args.role === 'the_user'
    ) {
      return res(ctx.status(500), ctx.delay(500));
    }

    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 1 &&
      reqBody.args[0].type === 'snowflake_create_function_permission'
    ) {
      metadata.sources[0].functions[0].permissions.push({
        role: reqBody.args[0].args.role,
      });
      return res(ctx.status(200), ctx.delay(500));
    }

    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 1 &&
      reqBody.args[0].type === 'snowflake_drop_function_permission'
    ) {
      metadata.sources[0].functions[0].permissions =
        metadata.sources[0].functions[0].permissions.filter(
          p => p.role !== reqBody.args[0].args.role
        );
      return res(ctx.status(200), ctx.delay(500));
    }

    return res(ctx.status(400));
  }),
];
