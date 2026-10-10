import { areTablesEqual, areTablesEqualCoalesce } from './predicate';

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
    },
  );

  it('returns false when arrays differ in length', () => {
    expect(areTablesEqual(['public', 'Album'], ['Album'])).toBe(false);
  });

  it('returns true for equal two-element arrays', () => {
    expect(areTablesEqual(['public', 'Album'], ['public', 'Album'])).toBe(true);
  });

  it('returns false when objects have different key counts', () => {
    expect(
      areTablesEqual({ name: 'Album' }, { name: 'Album', schema: 'public' }),
    ).toBe(false);
  });

  it('returns false when comparing an array to an object', () => {
    expect(
      areTablesEqual(['public', 'Album'], { schema: 'public', name: 'Album' }),
    ).toBe(false);
  });

  it('does NOT normalize differing shapes (schema object vs GDC array)', () => {
    // Documented gotcha: areTablesEqual is a raw structural comparison, so the
    // same table in two representations compares unequal.
    expect(
      areTablesEqual(['public', 'Album'], { schema: 'public', name: 'Album' }),
    ).toBe(false);
  });
});

describe('areTablesEqualCoalesce', () => {
  it('treats a GDC array and a schema object for the same table as equal', () => {
    expect(
      areTablesEqualCoalesce(['public', 'Album'], {
        schema: 'public',
        name: 'Album',
      }),
    ).toBe(true);
  });

  it('treats a dataset table and a schema table for the same table as equal', () => {
    expect(
      areTablesEqualCoalesce(
        { dataset: 'public', name: 'Album' },
        { schema: 'public', name: 'Album' },
      ),
    ).toBe(true);
  });

  it('returns false when names differ', () => {
    expect(
      areTablesEqualCoalesce(
        { schema: 'public', name: 'Album' },
        { schema: 'public', name: 'Artist' },
      ),
    ).toBe(false);
  });

  it('returns false when schemas differ', () => {
    expect(
      areTablesEqualCoalesce(
        { schema: 'public', name: 'Album' },
        { schema: 'private', name: 'Album' },
      ),
    ).toBe(false);
  });

  it('is falsy when either side cannot be normalized', () => {
    expect(areTablesEqualCoalesce({ foo: 'bar' }, { foo: 'bar' })).toBeFalsy();
  });
});
