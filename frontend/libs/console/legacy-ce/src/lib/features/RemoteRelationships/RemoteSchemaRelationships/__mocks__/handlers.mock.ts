import { rest } from 'msw';

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
  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    const body = req.body as Record<string, any>;

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'source_remote_schema'
    ) {
      return res(ctx.json(schema));
    }

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'with_default_values'
    ) {
      return res(ctx.json(schema));
    }

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'remoteSchema2'
    ) {
      return res(ctx.json(countries));
    }
    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'remoteSchema3'
    ) {
      return res(ctx.json(schema));
    }
    if (body.type === 'create_remote_schema_remote_relationship') {
      return res(ctx.json({ message: 'success' }));
    }
    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'countries'
    ) {
      return res(ctx.json(countries));
    }

    if (body.type === 'export_metadata') {
      return res(ctx.json(metadata));
    }

    if (body.type === 'create_remote_schema_remote_relationship') {
      return res(ctx.json({ message: 'success' }));
    }

    if (body.type === 'pg_create_object_relationship') {
      return res(ctx.json({ message: 'success' }));
    }

    if (body.type === 'pg_create_array_relationship') {
      return res(ctx.json({ message: 'success' }));
    }

    return res(ctx.json([{ message: 'success' }]));
  }),

  rest.post(`${url}/v2/query`, (req, res, ctx) => {
    const reqSql: string = (req?.body as Record<string, any>)?.args?.sql;
    if (reqSql.toLowerCase().includes('album')) {
      return res(ctx.json(albumTableColumnsResult));
    } else if (reqSql.toLowerCase().includes('address')) {
      return res(ctx.json(userAddressTableColumnsResult));
    } else if (reqSql.toLowerCase().includes('artist')) {
      return res(ctx.json(artistTableColumnsResult));
    }
    return res(ctx.json(userInfoTableColumnsResult));
  }),
];
