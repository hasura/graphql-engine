import {
  extractTableInfo,
  getTableDisplayName,
  getTableLabel,
  isDatasetTable,
  isGDCTable,
  isSchemaTable,
} from './table';

describe('isSchemaTable', () => {
  it('is true for a { schema, name } object', () => {
    expect(isSchemaTable({ schema: 'public', name: 'users' })).toBe(true);
  });

  it('is false for a dataset object, array, string and null', () => {
    expect(isSchemaTable({ dataset: 'ds', name: 'users' })).toBe(false);
    expect(isSchemaTable(['public', 'users'])).toBe(false);
    expect(isSchemaTable('users')).toBe(false);
    expect(isSchemaTable(null)).toBe(false);
  });
});

describe('isDatasetTable', () => {
  it('is true for a { dataset, name } object', () => {
    expect(isDatasetTable({ dataset: 'ds', name: 'users' })).toBe(true);
  });

  it('is false for a schema object and non-objects', () => {
    expect(isDatasetTable({ schema: 'public', name: 'users' })).toBe(false);
    expect(isDatasetTable(null)).toBe(false);
    expect(isDatasetTable('users')).toBe(false);
  });
});

describe('isGDCTable', () => {
  it('is true for an array of strings', () => {
    expect(isGDCTable(['public', 'users'])).toBe(true);
    expect(isGDCTable(['users'])).toBe(true);
  });

  it('is false for arrays containing non-strings', () => {
    expect(isGDCTable([1, 2])).toBe(false);
    expect(isGDCTable(['public', 3])).toBe(false);
  });

  it('is false for non-array inputs', () => {
    expect(isGDCTable({ schema: 'public', name: 'users' })).toBe(false);
    expect(isGDCTable('users')).toBe(false);
  });
});

describe('extractTableInfo', () => {
  it('returns a schema table unchanged', () => {
    expect(extractTableInfo({ schema: 'public', name: 'users' })).toEqual({
      schema: 'public',
      name: 'users',
    });
  });

  it('maps a dataset table onto { schema, name }', () => {
    expect(extractTableInfo({ dataset: 'ds', name: 'users' })).toEqual({
      schema: 'ds',
      name: 'users',
    });
  });

  it('maps a two-element GDC array onto { schema, name }', () => {
    expect(extractTableInfo(['public', 'users'])).toEqual({
      schema: 'public',
      name: 'users',
    });
  });

  it('maps a single-element GDC array with an empty schema', () => {
    expect(extractTableInfo(['users'])).toEqual({ schema: '', name: 'users' });
  });

  it('maps a bare string with an empty schema', () => {
    expect(extractTableInfo('users' as never)).toEqual({
      schema: '',
      name: 'users',
    });
  });

  it('returns null for an unrecognized shape', () => {
    expect(extractTableInfo({ foo: 'bar' } as never)).toBeNull();
  });
});

describe('getTableLabel', () => {
  it('builds a "source / schema / name" label for schema tables', () => {
    expect(
      getTableLabel({
        dataSourceName: 'chinook',
        table: { schema: 'public', name: 'Album' },
      }),
    ).toBe('chinook / public / Album');
  });

  it('builds a "source / dataset / name" label for dataset tables', () => {
    expect(
      getTableLabel({
        dataSourceName: 'chinook',
        table: { dataset: 'analytics', name: 'Album' },
      }),
    ).toBe('chinook / analytics / Album');
  });

  it('builds a label for GDC array tables', () => {
    expect(
      getTableLabel({ dataSourceName: 'chinook', table: ['public', 'Album'] }),
    ).toBe('chinook / public /Album');
  });

  it('returns an empty string for an unrecognized shape', () => {
    expect(
      getTableLabel({
        dataSourceName: 'chinook',
        table: { foo: 'bar' } as never,
      }),
    ).toBe('');
  });
});

describe('getTableDisplayName', () => {
  describe('when table is array', () => {
    it('returns the table name', () => {
      expect(getTableDisplayName(['public', 'name'])).toBe('public.name');
    });
  });

  describe('when table is null', () => {
    it('returns "Empty Object"', () => {
      expect(getTableDisplayName(null)).toBe('Empty Object');
    });
  });

  describe('when table is undefined', () => {
    it('returns "Empty Object"', () => {
      expect(getTableDisplayName(undefined)).toBe('Empty Object');
    });
  });

  describe('when table is string', () => {
    it('returns the value of table', () => {
      expect(getTableDisplayName('aTable')).toBe('aTable');
    });
  });

  describe('when table is object and includes "name"', () => {
    it('returns .name if object has a schema key (Postgres)', () => {
      expect(getTableDisplayName({ name: 'aName' })).toBe('aName');
    });
    it('returns name and other keys concatenated (non Postgres DBs)', () => {
      expect(getTableDisplayName({ name: 'aName', dataset: 'aDataset' })).toBe(
        'aDataset.aName',
      );
    });
  });

  describe('when table is object without name', () => {
    it('returns the values concatenated', () => {
      expect(
        getTableDisplayName({ collection: 'aCollection', dataset: 'chinook' }),
      ).toBe('aCollection.chinook');
    });

    it('returns the values deterministically concatenated', () => {
      expect(
        getTableDisplayName({ dataset: 'chinook', collection: 'aCollection' }),
      ).toBe('aCollection.chinook');
    });
  });

  describe('when table is a number', () => {
    it('returns the number as string', () => {
      expect(getTableDisplayName(1)).toBe('1');
    });
  });

  describe('when table is an empty array', () => {
    it('returns "Empty Object"', () => {
      expect(getTableDisplayName([])).toBe('Empty Object');
    });
  });

  describe('when table is a single-element array', () => {
    it('returns just the name', () => {
      expect(getTableDisplayName(['Album'])).toBe('Album');
    });
  });

  describe('when table is a schema object', () => {
    it('joins schema and name', () => {
      expect(getTableDisplayName({ schema: 'public', name: 'Album' })).toBe(
        'public.Album',
      );
    });
  });

  describe('when table has table_name/table_schema', () => {
    it('joins table_schema and table_name', () => {
      expect(
        getTableDisplayName({ table_name: 'Album', table_schema: 'public' }),
      ).toBe('public.Album');
    });
  });

  describe('wrapCharacter and separator', () => {
    it('wraps each part with the wrap character', () => {
      expect(
        getTableDisplayName({ schema: 'public', name: 'Album' }, '"'),
      ).toBe('"public"."Album"');
    });

    it('wraps a name-only result', () => {
      expect(getTableDisplayName({ name: 'Album' }, '`')).toBe('`Album`');
    });

    it('honours a custom separator', () => {
      expect(
        getTableDisplayName({ schema: 'public', name: 'Album' }, '', ' / '),
      ).toBe('public / Album');
    });
  });

  describe('fallback for arbitrary objects', () => {
    it('falls back to JSON.stringify when values are not all strings', () => {
      expect(getTableDisplayName({ id: 1, active: true })).toBe(
        JSON.stringify({ id: 1, active: true }),
      );
    });
  });
});
