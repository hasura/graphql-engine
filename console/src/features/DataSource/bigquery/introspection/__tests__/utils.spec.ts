import { RunSQLResponse } from '../../../api';
import { IntrospectedTable } from '../../../types';
import { adaptIntrospectedBigQueryTables } from '../utils';

describe('adaptIntrospectedBigQueryTables', () => {
  it('adapts the sql response', () => {
    const runSqlResponse: RunSQLResponse = {
      result_type: 'TuplesOk',
      result: [
        ['table_name', 'table_schema', 'table_type'],
        ['Artist', 'public', 'BASE TABLE'],
        ['Album', 'public', 'BASE TABLE'],
      ],
    };

    const expectedResponse: IntrospectedTable[] = [
      {
        name: 'public.Artist',
        table: {
          name: 'Artist',
          dataset: 'public',
        },
        type: 'BASE TABLE',
      },
      {
        name: 'public.Album',
        table: {
          name: 'Album',
          dataset: 'public',
        },
        type: 'BASE TABLE',
      },
    ];

    expect(adaptIntrospectedBigQueryTables(runSqlResponse)).toEqual(
      expectedResponse
    );
  });
});
