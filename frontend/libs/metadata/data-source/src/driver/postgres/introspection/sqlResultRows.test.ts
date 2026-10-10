import { parsePostgresTextArray, sqlResultToRows } from './sqlResultRows';

describe('sqlResultToRows', () => {
  it('keys each row by the header row', () => {
    expect(
      sqlResultToRows([
        ['a', 'b'],
        ['1', '2'],
        ['3', '4'],
      ]),
    ).toEqual([
      { a: '1', b: '2' },
      { a: '3', b: '4' },
    ]);
  });

  it('returns [] without data rows', () => {
    expect(sqlResultToRows(undefined)).toEqual([]);
    expect(sqlResultToRows(null)).toEqual([]);
    expect(sqlResultToRows([['a']])).toEqual([]);
  });
});

describe('parsePostgresTextArray', () => {
  it('parses unquoted elements', () => {
    expect(parsePostgresTextArray('{id}')).toEqual(['id']);
    expect(parsePostgresTextArray('{tenant_id,email}')).toEqual([
      'tenant_id',
      'email',
    ]);
  });

  it('parses quoted elements with escapes', () => {
    expect(
      parsePostgresTextArray('{"first name","a,b","q\\"x","b\\\\s","{}",NULL}'),
    ).toEqual(['first name', 'a,b', 'q"x', 'b\\s', '{}']);
  });

  it('keeps a quoted "NULL" string', () => {
    expect(parsePostgresTextArray('{"NULL"}')).toEqual(['NULL']);
  });

  it('returns [] for empty or invalid input', () => {
    expect(parsePostgresTextArray('{}')).toEqual([]);
    expect(parsePostgresTextArray(undefined)).toEqual([]);
    expect(parsePostgresTextArray('id')).toEqual([]);
  });
});
