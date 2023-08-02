import { Tables } from '../types';
import { getPermissionTypes } from './typeProviderHelpers';

describe('RowPermissionInput -> updateKey should', () => {
  test('create root value object', () => {
    const tables = [
      {
        table: {
          dataset: 'bigquery_sample',
          name: 'sample_table',
        },
        relationships: [
          {
            name: 'bq_test_relation',
            type: 'object',
            table: {
              dataset: 'bigquery_sample',
              name: 'sample_table',
              type: 'object',
            },
          },
          {
            name: 'test_2',

            table: {
              dataset: 'bigquery_sample',
              name: 'sample_table',
            },
          },
        ],
        columns: [
          { name: 'Series_reference', type: 'STRING' },
          { name: 'Period', type: 'FLOAT64' },
          { name: 'Data_value', type: 'FLOAT64' },
          { name: 'Suppressed', type: 'BOOL' },
          { name: 'STATUS', type: 'STRING' },
          { name: 'UNITS', type: 'STRING' },
          { name: 'Magnitude', type: 'INT64' },
          { name: 'Subject', type: 'STRING' },
          { name: 'Group', type: 'STRING' },
          { name: 'Series_title_1', type: 'STRING' },
          { name: 'Series_title_2', type: 'STRING' },
          { name: 'Series_title_3', type: 'STRING' },
          { name: 'Series_title_4', type: 'STRING' },
          { name: 'Series_title_5', type: 'STRING' },
        ],
      },
    ] as Tables;
    const table = { dataset: 'bigquery_sample', name: 'sample_table' };
    const permissions = { Series_reference: { _eq: '' } };
    const result = getPermissionTypes(tables, table, permissions);

    expect(result).toEqual({});
  });
});
