import { http, HttpResponse } from 'msw';

import { schema } from './schema';
import { countries } from './countries_schema';
import { metadata } from './metadata';
import {
  albumTableColumnsResult,
  userAddressTableColumnsResult,
  userInfoTableColumnsResult,
  artistTableColumnsResult,
} from './tables';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  http.post(`${url}/v1/metadata`, async ({ request }) => {
    const body = (await request.json()) as Record<string, any>;

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'source_remote_schema'
    ) {
      return HttpResponse.json(schema);
    }

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'with_default_values'
    ) {
      return HttpResponse.json(schema);
    }

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'remoteSchema2'
    ) {
      return HttpResponse.json(countries);
    }
    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'remoteSchema3'
    ) {
      return HttpResponse.json(schema);
    }
    if (body.type === 'create_remote_schema_remote_relationship') {
      return HttpResponse.json({ message: 'success' });
    }
    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'countries'
    ) {
      return HttpResponse.json(countries);
    }

    if (body.type === 'export_metadata') {
      return HttpResponse.json(metadata);
    }

    if (body.type === 'create_remote_schema_remote_relationship') {
      return HttpResponse.json({ message: 'success' });
    }

    if (body.type === 'pg_create_object_relationship') {
      return HttpResponse.json({ message: 'success' });
    }

    if (body.type === 'pg_create_array_relationship') {
      return HttpResponse.json({ message: 'success' });
    }

    return HttpResponse.json([{ message: 'success' }]);
  }),

  http.post(`${url}/v2/query`, async ({ request }) => {
    const body = (await request.json()) as Record<string, any>;
    const reqSql: string = body?.args?.sql;
    if (reqSql.toLowerCase().includes('album')) {
      return HttpResponse.json(albumTableColumnsResult);
    } else if (reqSql.toLowerCase().includes('address')) {
      return HttpResponse.json(userAddressTableColumnsResult);
    } else if (reqSql.toLowerCase().includes('artist')) {
      return HttpResponse.json(artistTableColumnsResult);
    }
    return HttpResponse.json(userInfoTableColumnsResult);
  }),
];
