import {
  getNewRootPermissionState,
  getPrimaryKeysFromTable,
  hasSelectPrimaryKey,
  Schema,
  State,
} from './utils';

describe('getPrimaryKeysFromTable', () => {
  it('returns the primary keys for a table', () => {
    const schemas: Schema[] = [
      {
        table_name: 'foo',
        primary_key: {
          columns: ['id'],
        },
      },
      {
        table_name: 'bar',
        primary_key: {
          columns: ['userId'],
        },
      },
    ];
    const state: State = {
      permissionsState: {
        table: 'foo',
      },
    };
    const expected = ['id'];
    expect(getPrimaryKeysFromTable(schemas, state)).toEqual(expected);
  });

  describe('when the table is not present', () => {
    it('returns and empty array', () => {
      const schemas: Schema[] = [
        {
          table_name: 'bar',
          primary_key: {
            columns: ['userId'],
          },
        },
      ];
      const state: State = {
        permissionsState: {
          table: 'foo',
        },
      };
      const expected: string[] = [];
      expect(getPrimaryKeysFromTable(schemas, state)).toEqual(expected);
    });
  });
});

describe('hasSelectPrimaryKey', () => {
  describe('when every primary key is selected', () => {
    it('returns true', () => {
      const primaryKeys = ['id', 'name'];
      const selectedColumns = ['id', 'name'];
      expect(hasSelectPrimaryKey(primaryKeys, selectedColumns)).toBe(true);
    });
  });
  describe('when some primary key are selected', () => {
    it('returns true', () => {
      const primaryKeys = ['id', 'name'];
      const selectedColumns = ['id'];
      expect(hasSelectPrimaryKey(primaryKeys, selectedColumns)).toBe(false);
    });
  });
});

describe('getNewRootPermissionState', () => {
  describe('when query is not allowed, and the altered permission is included', () => {
    it('returns the permissions without the altered permissions', () => {
      expect(
        getNewRootPermissionState(
          ['select', 'select_aggregate', 'select_by_pk'],
          'select_by_pk',
          false
        )
      ).toEqual(['select', 'select_aggregate']);
    });
  });

  describe('when query is allowed', () => {
    it('returns the same permissions', () => {
      expect(
        getNewRootPermissionState(
          ['select', 'select_aggregate', 'select_by_pk'],
          'select_by_pk',
          true
        )
      ).toEqual(['select', 'select_aggregate', 'select_by_pk']);
    });
  });
});
