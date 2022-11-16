import { getTableName } from './getTableName';

describe('getTableName', () => {
  test.each`
    table                                   | databaseHierarchy      | expectedName
    ${{ name: 'Album', schema: 'public' }}  | ${['schema', 'name']}  | ${'public.Album'}
    ${{ name: 'Album', dataset: 'public' }} | ${['dataset', 'name']} | ${'public.Album'}
    ${['public', 'Album']}                  | ${[]}                  | ${'public.Album'}
  `(
    'when invoked for $table & $hierarchy, should return $expectedName',
    ({ table, databaseHierarchy, expectedName }) => {
      const result = getTableName(table, databaseHierarchy);
      expect(result).toBe(expectedName);
    }
  );

  it('should throw error, if hierachy is wrong', () => {
    try {
      // TS error is hidden because we're checking for an error
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = getTableName({ name: 'Album', schema: 'public' }, [
        'dataset',
        'name',
      ]);
    } catch (err: unknown) {
      expect((err as Error).message).toEqual('unable to find hierachy value');
    }
  });

  it('should throw error, if hierachy is empty and the table is an object', () => {
    try {
      // TS error is hidden because we're checking for an error
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const result = getTableName({ name: 'Album', schema: 'public' }, []);
    } catch (err: unknown) {
      expect((err as Error).message).toEqual('No database hierarchy found');
    }
  });

  test.each`
    tableObject  | databaseHierarchy
    ${null}      | ${['schema', 'name']}
    ${undefined} | ${['dataset', 'name']}
  `(
    'should throw error, if table is %s',
    ({ tableObject, databaseHierarchy }) => {
      try {
        // TS error is hidden because we're checking for an error
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const result = getTableName(tableObject, databaseHierarchy);
      } catch (err: unknown) {
        expect((err as Error).message).toEqual(
          'Table cannot be null or undefined'
        );
      }
    }
  );

  it.each`
    tableObject | databaseHierarchy
    ${1}        | ${[]}
    ${123.5}    | ${[]}
  `(
    'should throw error, if table is %s',
    ({ tableObject, databaseHierarchy }) => {
      try {
        // TS error is hidden because we're checking for an error
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const result = getTableName(tableObject, databaseHierarchy);
      } catch (err: unknown) {
        expect((err as Error).message).toEqual(
          'Table name could be generated for the given table type'
        );
      }
    }
  );
});
