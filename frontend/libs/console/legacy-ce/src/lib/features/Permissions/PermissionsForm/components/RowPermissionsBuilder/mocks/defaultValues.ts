import { TableColumn } from '../../../../../DataSource';
import { MetadataDataSource } from '../../../../../../metadata/types';
export const tableColumns = [
  {
    name: 'Series_reference',
    dataType: 'STRING',
    consoleDataType: 'string',
    nullable: false,
  },
  {
    name: 'Period',
    dataType: 'FLOAT64',
    consoleDataType: 'number',
    nullable: false,
  },
  {
    name: 'Data_value',
    dataType: 'FLOAT64',
    consoleDataType: 'number',
    nullable: false,
  },
] as TableColumn[];

export const sourceMetadata = {
  name: 'bigquery_test1',
  kind: 'bigquery',
  tables: [
    {
      table: { dataset: 'bigquery_sample', name: 'sample_table' },
      object_relationships: [
        {
          name: 'bq_test_relation',
          using: {
            manual_configuration: {
              column_mapping: { Period: 'STATUS' },
              remote_table: {
                dataset: 'bigquery_sample',
                name: 'sample_table',
              },
            },
          },
        },
        {
          name: 'test_2',
          using: {
            manual_configuration: {
              column_mapping: { Period: 'Series_title_2' },
              insertion_order: null,
              remote_table: {
                dataset: 'bigquery_sample',
                name: 'sample_table',
              },
            },
          },
        },
      ],
      select_permissions: [
        {
          role: 'user',
          permission: {
            columns: ['Series_reference', 'Period', 'Data_value'],
            filter: {
              _and: [{ Series_reference: { _eq: 'X-Hasura-User-Id' } }],
            },
          },
        },
      ],
    },
  ],
  configuration: {
    datasets: ['bigquery_sample'],
    global_select_limit: 1,
    project_id: 'sensei',
    service_account: {
      client_email: '@mail.com',
      private_key: '',
      project_id: 'sensei',
    },
  },
} as MetadataDataSource;
