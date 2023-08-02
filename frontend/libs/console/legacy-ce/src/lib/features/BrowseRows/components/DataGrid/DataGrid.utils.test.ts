import {
  TableColumn,
  TableRow,
  WhereClause,
} from '../../../../features/DataSource';
import { DataGridOptions } from './DataGrid';
import {
  adaptSelectedRowIdsToWhereClause,
  AdaptSelectedRowIdsToWhereClauseArgs,
  mapWhereAndSortConditions,
  FilterConditions,
  replaceFiltersInUrl,
  applyWhereAndSortConditionsToQueryString,
  convertUrlToDataGridOptions,
  convertValueToGraphQL,
} from './DataGrid.utils';

describe('adaptSelectedRowIdsToWhereClause', () => {
  it('adapts selected rows', () => {
    const rowsId: AdaptSelectedRowIdsToWhereClauseArgs['rowsId'] = {
      0: true,
      3: true,
    };

    const rows: TableRow[] = [
      {
        ArtistId: 1,
        Name: 'The Beatles',
      },
      {
        ArtistId: 2,
        Name: 'AC/DC',
      },
      {
        ArtistId: 3,
        Name: 'Aerosmith',
      },
      {
        ArtistId: 4,
        Name: 'Audioslave',
      },
      {
        ArtistId: 5,
        Name: '	Billy Cobham',
      },
    ];

    const primaryKeys = ['ArtistId'];

    const expected: WhereClause[] = [
      {
        ArtistId: { _in: [1, 4] },
      },
    ];
    expect(
      adaptSelectedRowIdsToWhereClause({
        rowsId,
        rows,
        primaryKeys,
      })
    ).toEqual(expected);
  });
});

describe('mapWhereAndSortConditions', () => {
  describe('when where and sort conditions are defined', () => {
    it('returns the query string', () => {
      const options: DataGridOptions = {
        limit: 0,
        offset: 0,
        where: [
          {
            AlbumId: {
              _gte: 2,
            },
          },
          {
            Title: {
              _like: '%foo%',
            },
          },
        ],
        order_by: [
          {
            column: 'AlbumId',
            type: 'desc',
          },
          {
            column: 'Title',
            type: 'asc',
          },
        ],
      };

      expect(mapWhereAndSortConditions(options)).toEqual([
        {
          filter: 'AlbumId;_gte;2',
        },
        {
          filter: 'Title;_like;%foo%',
        },
        {
          sort: 'AlbumId;desc',
        },
        {
          sort: 'Title;asc',
        },
      ]);
    });
  });

  describe('when only where conditions are defined', () => {
    it('returns the query string', () => {
      const options: DataGridOptions = {
        limit: 0,
        offset: 0,
        where: [
          {
            AlbumId: {
              _gte: 2,
            },
          },
          {
            Title: {
              _like: '%foo%',
            },
          },
        ],
      };

      expect(mapWhereAndSortConditions(options)).toEqual([
        {
          filter: 'AlbumId;_gte;2',
        },
        {
          filter: 'Title;_like;%foo%',
        },
      ]);
    });
  });

  describe('when only sort conditions are defined', () => {
    it('returns the query string', () => {
      const options: DataGridOptions = {
        limit: 0,
        offset: 0,
        order_by: [
          {
            column: 'AlbumId',
            type: 'desc',
          },
          {
            column: 'Title',
            type: 'asc',
          },
        ],
      };

      expect(mapWhereAndSortConditions(options)).toEqual([
        {
          sort: 'AlbumId;desc',
        },
        {
          sort: 'Title;asc',
        },
      ]);
    });
  });
});

describe('replaceFiltersInUrl', () => {
  describe('when filter and sort conditions are provided', () => {
    it('returns the query string', () => {
      const filterConditions: FilterConditions = [
        { filter: 'AlbumId;_gte;1' },
        { filter: 'Title;_like;%foo%' },
        { sort: 'AlbumId;asc' },
      ];

      expect(
        replaceFiltersInUrl('?database=Chinook&table=Album', filterConditions)
      ).toBe(
        'database=Chinook&table=Album&filter=AlbumId%3B_gte%3B1&filter=Title%3B_like%3B%25foo%25&sort=AlbumId%3Basc'
      );
    });
  });
});

describe('applyWhereAndSortConditionsToQueryString', () => {
  it('returns the query string', () => {
    const options: DataGridOptions = {
      limit: 0,
      offset: 0,
      where: [
        {
          AlbumId: {
            _gte: 2,
          },
        },
        {
          Title: {
            _like: '%foo%',
          },
        },
      ],
      order_by: [
        {
          column: 'AlbumId',
          type: 'desc',
        },
        {
          column: 'Title',
          type: 'asc',
        },
      ],
    };

    const search = '?database=Chinook&table=%5B%22Album%22%5D';

    expect(
      applyWhereAndSortConditionsToQueryString({
        options,
        search,
      })
    ).toBe(
      'database=Chinook&table=%5B%22Album%22%5D&filter=AlbumId%3B_gte%3B2&filter=Title%3B_like%3B%25foo%25&sort=AlbumId%3Bdesc&sort=Title%3Basc'
    );
  });
});

describe('convertUrlToDataGridOptions', () => {
  describe('when filters and sort are defined', () => {
    it('returns the options', () => {
      const search =
        'database=Chinook&table=Album&filter=AlbumId%3B_gte%3B1&filter=Title%3B_like%3B%25foo%25&sort=AlbumId%3Basc';

      const expected: DataGridOptions = {
        where: [
          {
            AlbumId: {
              _gte: '1',
            },
          },
          {
            Title: {
              _like: '%foo%',
            },
          },
        ],
        order_by: [
          {
            column: 'AlbumId',
            type: 'asc',
          },
        ],
      };
      expect(convertUrlToDataGridOptions(search)).toEqual(expected);
    });
  });

  describe('when filters are defined', () => {
    it('returns the options', () => {
      const search =
        'database=Chinook&table=Album&filter=AlbumId%3B_gte%3B1&filter=Title%3B_like%3B%25foo%25';

      const expected: DataGridOptions = {
        where: [
          {
            AlbumId: {
              _gte: '1',
            },
          },
          {
            Title: {
              _like: '%foo%',
            },
          },
        ],
        order_by: [],
      };
      expect(convertUrlToDataGridOptions(search)).toEqual(expected);
    });
  });

  describe('when sort are defined', () => {
    it('returns the options', () => {
      const search =
        'database=Chinook&table=Album&sort=AlbumId%3Basc&sort=Title%3Bdesc';

      const expected: DataGridOptions = {
        where: [],
        order_by: [
          {
            column: 'AlbumId',
            type: 'asc',
          },
          {
            column: 'Title',
            type: 'desc',
          },
        ],
      };
      expect(convertUrlToDataGridOptions(search)).toEqual(expected);
    });
  });

  describe('when filters and sort are not defined', () => {
    it('returns the options', () => {
      const search = 'database=Chinook&table=Album';

      const expected: DataGridOptions = {
        where: [],
        order_by: [],
      };
      expect(convertUrlToDataGridOptions(search)).toEqual(expected);
    });
  });

  describe('when table columns are provided', () => {
    it('returns the options', () => {
      const search =
        'database=Chinook&table=Album&filter=AlbumId%3B_gte%3B1&filter=Title%3B_like%3B%25foo%25&sort=AlbumId%3Basc';

      const expected: DataGridOptions = {
        where: [
          {
            AlbumId: {
              _gte: 1,
            },
          },
          {
            Title: {
              _like: '%foo%',
            },
          },
        ],
        order_by: [
          {
            column: 'AlbumId',
            type: 'asc',
          },
        ],
      };

      const tableColumns: TableColumn[] = [
        {
          name: 'AlbumId',
          dataType: 'number',
          graphQLProperties: { name: 'AlbumId', scalarType: 'decimal' },
          consoleDataType: 'number',
        },
        {
          name: 'Title',
          dataType: 'string',
          graphQLProperties: { name: 'Title', scalarType: 'String' },
          consoleDataType: 'string',
        },
      ];

      expect(convertUrlToDataGridOptions(search, tableColumns)).toEqual(
        expected
      );
    });
  });
});

describe('convertValueToGraphQL', () => {
  it('converts decimal', () => {
    const value = '1';
    const tableColumn: TableColumn = {
      name: 'AlbumId',
      dataType: 'number',
      consoleDataType: 'number',
      graphQLProperties: {
        name: 'AlbumId',
        scalarType: 'decimal',
      },
    };
    expect(convertValueToGraphQL(value, tableColumn)).toBe(1);
  });

  it('converts float', () => {
    const value = '1';
    const tableColumn: TableColumn = {
      name: 'AlbumId',
      dataType: 'number',
      consoleDataType: 'number',
      graphQLProperties: {
        name: 'AlbumId',
        scalarType: 'float',
      },
    };
    expect(convertValueToGraphQL(value, tableColumn)).toBe(1);
  });

  it('converts boolean', () => {
    const value = 'true';
    const tableColumn: TableColumn = {
      name: 'AlbumId',
      dataType: 'bool',
      consoleDataType: 'number',
      graphQLProperties: {
        name: 'AlbumId',
        scalarType: 'boolean',
      },
    };
    expect(convertValueToGraphQL(value, tableColumn)).toBe(true);
  });

  it('converts string', () => {
    const value = 'aString';
    const tableColumn: TableColumn = {
      name: 'AlbumId',
      dataType: 'string',
      consoleDataType: 'number',
      graphQLProperties: {
        name: 'AlbumId',
        scalarType: 'string',
      },
    };
    expect(convertValueToGraphQL(value, tableColumn)).toBe('aString');
  });

  it('converts array of strings', () => {
    const value = '[1, 2, 3, 4]';
    const tableColumn: TableColumn = {
      name: 'AlbumId',
      dataType: 'string',
      consoleDataType: 'string',
      graphQLProperties: {
        name: 'AlbumId',
        scalarType: 'string',
      },
    };
    expect(convertValueToGraphQL(value, tableColumn)).toBe('["1","2","3","4"]');
  });

  it('converts array of int', () => {
    const value = '[1, 2, 3, 4]';
    const tableColumn: TableColumn = {
      name: 'AlbumId',
      dataType: 'number',
      consoleDataType: 'number',
      graphQLProperties: {
        name: 'AlbumId',
        scalarType: 'int',
      },
    };
    expect(convertValueToGraphQL(value, tableColumn)).toBe('[1,2,3,4]');
  });

  it('converts array of float', () => {
    const value = '[1.1, 2.2, 3.3, 4.4]';
    const tableColumn: TableColumn = {
      name: 'AlbumId',
      dataType: 'number',
      consoleDataType: 'number',
      graphQLProperties: {
        name: 'AlbumId',
        scalarType: 'float',
      },
    };
    expect(convertValueToGraphQL(value, tableColumn)).toBe('[1.1,2.2,3.3,4.4]');
  });
});
