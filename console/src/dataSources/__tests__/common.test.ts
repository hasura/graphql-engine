import { escapeTableName, escapeTableColumns } from '../common';
import { Table } from '../types';

describe('Table and column names escaping', () => {
  it('Escape Column Name', () => {
    const input1 = {
      columns: [
        { column_name: 'fine' },
        { column_name: 'author_name' },
        { column_name: 'invalid&name' },
        { column_name: '10_number' },
        { column_name: '$a' },
      ],
    };
    const expected1 = {
      'invalid&name': 'invalid_name',
      '10_number': 'column_10_number',
      $a: '_a',
    };
    expect(escapeTableColumns(input1 as Table)).toStrictEqual(expected1);
    const input2 = {
      columns: [
        { column_name: 'fine' },
        { column_name: 'author_name' },
        { column_name: '10_number' },
      ],
    };
    expect(escapeTableColumns(input2 as Table)).toStrictEqual({
      '10_number': 'column_10_number',
    });
  });

  it('Escape Column Name', () => {
    const cases: Array<[string, string | null]> = [
      ['10_users_$_table', 'table_10_users_table'],
      ['valid_table_name', null],
      ['Author Table', 'author_table'],
      ['', null],
      ['valid_10_taable', null],
    ];
    cases.forEach(([input, expected]) => {
      expect(escapeTableName(input)).toEqual(expected);
    });
  });
});
