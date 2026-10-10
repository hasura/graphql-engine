import { http, HttpResponse } from 'msw';
import {
  metadata,
  capabilitiesResponse,
  config,
  sourceKinds,
  tableInfo,
  sourceTables,
  introspection,
} from './dataStubs';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  http.get(`${url}/v1alpha1/config`, async () => {
    return HttpResponse.json(config);
  }),
  http.post(`${url}/v2/query`, async ({ request }) => {
    const body = (await request.json()) as Record<string, any>;

    const isUseSchemaList = body?.args?.sql?.includes('SELECT schema_name');
    const isColumnsQuery = body?.args?.sql?.includes('column_name');

    if (isUseSchemaList) {
      return HttpResponse.json({
        result_type: 'TuplesOk',
        result: [['schema_name'], ['public'], ['default']],
      });
    }

    if (isColumnsQuery) {
      return HttpResponse.json({
        result_type: 'TuplesOk',
        result: [
          ['column_name', 'data_type'],
          ['id', 'integer'],
          ['name', 'text'],
          ['email', 'text'],
        ],
      });
    }

    return HttpResponse.json({
      result_type: 'TuplesOk',
      result: [
        ['tables'],
        [
          '[{"table_schema":"public","table_name":"a_table","table_type":"TABLE","comment":"comment","columns":[{"comment": null, "data_type": "integer", "table_name": "a_table", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'a_table_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": "column comment", "data_type": "text", "table_name": "a_table", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}, {"table_schema":"public","table_name":"users","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "users", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'users_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "email", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 3}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "type", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 5}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "username", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 4}],"triggers":[],"view_info":null}]',
        ],
      ],
    });
  }),
  http.post(`${url}/v1/metadata`, async ({ request }) => {
    const body = (await request.json()) as Record<string, any>;

    if (body.type === 'get_source_kind_capabilities') {
      return HttpResponse.json(capabilitiesResponse);
    }

    if (body.type === 'list_source_kinds') {
      return HttpResponse.json(sourceKinds);
    }

    if (body.type === 'get_table_info') {
      return HttpResponse.json(tableInfo);
    }

    if (body.type === 'dataconnector_get_source_tables') {
      return HttpResponse.json(sourceTables);
    }
    if (body.type === 'export_metadata') {
      return HttpResponse.json(metadata);
    }

    return HttpResponse.json([{ message: 'success' }]);
  }),
  http.post(`${url}/v1/graphql`, () => {
    return HttpResponse.json(introspection);
  }),
];
