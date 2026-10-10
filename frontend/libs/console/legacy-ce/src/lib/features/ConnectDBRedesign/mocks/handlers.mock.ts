import { http, HttpResponse, delay } from 'msw';
import {
  mockCapabilitiesResponse,
  mockMetadata,
  mockSourceKinds,
} from './data.mock';
type AgentTestType =
  | 'super_connector_agents_added'
  | 'super_connector_agents_not_added'
  | 'super_connector_agents_added_but_unavailable';

export const handlers = (props?: { agentTestType: AgentTestType }) => [
  http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
    const requestBody = (await request.json()) as Record<string, any>;
    if (requestBody.type === 'list_source_kinds') {
      switch (props?.agentTestType) {
        case 'super_connector_agents_added':
          return HttpResponse.json(mockSourceKinds.agentsAdded);
        case 'super_connector_agents_not_added':
          return HttpResponse.json(mockSourceKinds.agentsNotAdded);
        case 'super_connector_agents_added_but_unavailable':
          return HttpResponse.json(
            mockSourceKinds.agentsAddedSuperConnectorNotAvailable,
          );
        default:
          return HttpResponse.json(mockSourceKinds.agentsNotAdded);
      }
    }
    if (requestBody.type === 'export_metadata')
      return HttpResponse.json(mockMetadata);

    if (requestBody.type === 'get_source_kind_capabilities')
      return HttpResponse.json(mockCapabilitiesResponse);

    if (requestBody.type === 'get_inconsistent_metadata')
      return HttpResponse.json({
        inconsistent_objects: [
          {
            definition: 'bikes',
            message: {
              exception: {
                message:
                  '[Microsoft][ODBC Driver 17 for SQL Server]Login timeout expired',
                type: 'unsuccessful_return_code',
              },
            },
            name: 'source bikes',
            reason: 'Inconsistent object: mssql connection error',
            type: 'source',
          },
        ],
        is_consistent: false,
      });

    return HttpResponse.json({});
  }),
  http.get(`http://localhost:8080/v1alpha1/config`, async () => {
    await delay(3000);
    return HttpResponse.json({
      version: 'dev-fb2bab3-test-app',
      is_function_permissions_inferred: true,
      is_remote_schema_permissions_enabled: true,
      is_admin_secret_set: false,
      is_auth_hook_set: false,
      is_jwt_set: false,
      jwt: [],
      is_allow_list_enabled: false,
      live_queries: { batch_size: 100, refetch_delay: 1 },
      streaming_queries: { batch_size: 100, refetch_delay: 1 },
      console_assets_dir: null,
      experimental_features: ['naming_convention'],
      is_prometheus_metrics_enabled: false,
      default_naming_convention: null,
    });
  }),
  http.post('http://localhost:8080/v2/query', async ({ request }) => {
    const requestBody = (await request.json()) as Record<string, any>;

    if (
      requestBody.type === 'run_sql' &&
      JSON.stringify(requestBody.args) ===
        JSON.stringify({ sql: 'SELECT VERSION()', source: 'chinook' })
    )
      return HttpResponse.json({
        result_type: 'TuplesOk',
        result: [
          ['version'],
          [
            'PostgreSQL 12.12 (Debian 12.12-1.pgdg110+1) on x86_64-pc-linux-gnu, compiled by gcc (Debian 10.2.1-6) 10.2.1 20210110, 64-bit',
          ],
        ],
      });

    if (
      requestBody.type === 'mssql_run_sql' &&
      JSON.stringify(requestBody.args) ===
        JSON.stringify({
          sql: 'SELECT @@VERSION as version;',
          source: 'mssql1',
        })
    )
      return HttpResponse.json({
        result_type: 'TuplesOk',
        result: [
          ['version'],
          [
            'Microsoft SQL Server 2008 (SP1) - 10.0.2531.0 (X64)   Mar 29 2009 10:11:52   Copyright (c) 1988-2008 Microsoft Corporation  Express Edition (64-bit) on Windows NT 6.1 <X64> (Build 7600: )',
          ],
        ],
      });
    return HttpResponse.json({});
  }),
];
