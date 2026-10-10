import { http, HttpResponse } from 'msw';
import export_metadata from './export_metadata';
import get_source_kind_capabilities from './get_source_kind_capabilities';
import get_table_info from './get_table_info';
import { introspection } from './introspection';

export function handlers() {
  return [
    http.post('http://localhost:8080/v1/graphql', async () => {
      return HttpResponse.json(introspection);
    }),
    http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
      const body = await request.json();
      if (body.type === 'get_table_info') {
        return HttpResponse.json(get_table_info);
      }
      if (body.type === 'get_source_kind_capabilities')
        return HttpResponse.json(get_source_kind_capabilities);
      if (body.type === 'export_metadata') {
        return HttpResponse.json(export_metadata);
      }
      return HttpResponse.json({});
    }),
  ];
}
