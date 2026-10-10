import metadata from './metadata';
import { http, HttpResponse, delay } from 'msw';
import config from './config';

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
      reqBody.args.length === 1 &&
      reqBody.args[0].args.role === 'the_user'
    ) {
      await delay(500);
      return new HttpResponse(null, { status: 500 });
    }

    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 1 &&
      reqBody.args[0].type === 'snowflake_create_function_permission'
    ) {
      metadata.sources[0].functions[0].permissions.push({
        role: reqBody.args[0].args.role,
      });
      await delay(500);
      return new HttpResponse(null, { status: 200 });
    }

    if (
      reqBody.type === 'bulk' &&
      reqBody.args.length === 1 &&
      reqBody.args[0].type === 'snowflake_drop_function_permission'
    ) {
      metadata.sources[0].functions[0].permissions =
        metadata.sources[0].functions[0].permissions.filter(
          (p) => p.role !== reqBody.args[0].args.role,
        );
      await delay(500);
      return new HttpResponse(null, { status: 200 });
    }

    return new HttpResponse(null, { status: 400 });
  }),
];
