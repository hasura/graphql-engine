import type { TableColumn } from '../../driver/types';
import { escapeTableName, escapeTableColumnsMap } from '../table';

describe('Table and column names escaping', () => {
  it('Escape Column Name', () => {
    const input1 = {
      columns: [
        { name: 'fine' },
        { name: 'author_name' },
        { name: 'invalid&name' },
        { name: '10_number' },
        { name: '$a' },
      ] as TableColumn[],
    };
    const expected1 = {
      'invalid&name': 'invalid_name',
      '10_number': 'column_10_number',
      $a: '_a',
    };
    expect(escapeTableColumnsMap(input1.columns)).toStrictEqual(expected1);
    const input2 = {
      columns: [
        { name: 'fine' },
        { name: 'author_name' },
        { name: '10_number' },
      ] as TableColumn[],
    };
    expect(escapeTableColumnsMap(input2.columns)).toStrictEqual({
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
