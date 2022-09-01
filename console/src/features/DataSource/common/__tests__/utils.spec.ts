import { RunSQLResponse } from '../../api';
import { IntrospectedTable, TableColumn } from '../../types';
import { adaptIntrospectedTables, adaptTableColumns } from '../utils';

describe('adaptIntrospectedTables', () => {
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
          schema: 'public',
        },
        type: 'BASE TABLE',
      },
      {
        name: 'public.Album',
        table: {
          name: 'Album',
          schema: 'public',
        },
        type: 'BASE TABLE',
      },
    ];

    expect(adaptIntrospectedTables(runSqlResponse)).toEqual(expectedResponse);
  });
});

describe('adaptTableColumns', () => {
  it('adapts the sql response', () => {
    const runSqlResponse: RunSQLResponse = {
      result_type: 'TuplesOk',
      result: [
        ['column_name', 'data_type'],
        ['id', 'int'],
        ['name', 'varchar'],
        ['updated_at', 'datetime'],
      ],
    };

    const expectedResponse: TableColumn[] = [
      { name: 'id', dataType: 'int' },
      { name: 'name', dataType: 'varchar' },
      { name: 'updated_at', dataType: 'datetime' },
    ];

    expect(adaptTableColumns(runSqlResponse.result)).toEqual(expectedResponse);
  });
});
