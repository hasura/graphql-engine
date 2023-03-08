import { areTablesEqual } from './areTablesEqual';

describe('areTablesEqual', () => {
  it.each`
    table1                                 | table2                                  | expected
    ${undefined}                           | ${undefined}                            | ${true}
    ${null}                                | ${undefined}                            | ${false}
    ${1}                                   | ${2}                                    | ${false}
    ${['Album']}                           | ${['Album']}                            | ${true}
    ${{ name: 'Album', schema: 'public' }} | ${{ name: 'Artist', schema: 'public' }} | ${false}
    ${{ name: 'Album', schema: 'public' }} | ${{ name: 'Album', schema: 'public' }}  | ${true}
  `(
    'returns $expected when table1 is $table1 and table2 is $table2',
    ({ table1, table2, expected }) => {
      expect(areTablesEqual(table1, table2)).toEqual(expected);
    }
  );
});
