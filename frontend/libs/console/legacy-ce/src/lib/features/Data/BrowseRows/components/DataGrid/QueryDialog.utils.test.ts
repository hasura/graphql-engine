import { TableColumn } from '@hasura/metadata/data-source';
import { isIgnoredFilter, transformFilterValues } from './QueryDialog.utils';
import {
  applyWhereAndSortConditionsToQueryString,
  convertUrlToDataGridOptions,
} from './DataGrid.utils';

const column = (
  dataType: TableColumn['dataType'],
  consoleDataType: TableColumn['consoleDataType'],
  scalarType?: string,
): TableColumn => ({
  name: 'value',
  dataType,
  consoleDataType,
  graphQLProperties: { name: 'value', scalarType },
});

describe('isIgnoredFilter', () => {
  const nonTextColumns = [
    ['uuid', 'uuid', 'uuid'],
    ['integer', 'integer', 'Int'],
    ['float8', 'float', 'float8'],
    ['timestamptz', 'timestamp', 'timestamptz'],
    ['boolean', 'boolean', 'Boolean'],
    ['jsonb', 'json', 'jsonb'],
    // GDC reports numeric columns as 'string'.
    ['number', 'string', 'number'],
  ] as const;

  describe.each(nonTextColumns)(
    '%s columns',
    (dataType, consoleDataType, scalarType) => {
      const columns = [column(dataType, consoleDataType, scalarType)];

      it.each([null, undefined, '', '   '])('ignores %j values', (value) => {
        expect(
          isIgnoredFilter(columns, { column: 'value', operator: '$eq', value }),
        ).toBe(true);
      });

      it.each(['0', 'false', 0, false])('keeps %j values', (value) => {
        expect(
          isIgnoredFilter(columns, { column: 'value', operator: '$eq', value }),
        ).toBe(false);
      });
    },
  );

  it.each([
    ['text', 'text', 'String'],
    ['varchar', 'string', 'String'],
    ['custom_scalar', 'string', 'custom_scalar'],
  ] as const)(
    'keeps empty %s filters',
    (dataType, consoleDataType, scalarType) => {
      const columns = [column(dataType, consoleDataType, scalarType)];
      expect(
        isIgnoredFilter(columns, {
          column: 'value',
          operator: '$eq',
          value: '',
        }),
      ).toBe(false);
    },
  );

  it('keeps filters on unknown columns', () => {
    expect(
      isIgnoredFilter([column('uuid', 'uuid')], {
        column: 'unknown',
        operator: '$eq',
        value: '',
      }),
    ).toBe(false);
  });
});

describe('transformFilterValues', () => {
  it.each([
    ['uuid', 'uuid', '31b789b0-acfe-43a8-bdaf-561313e8f82e'],
    ['timestamptz', 'timestamp', '2026-10-22T10:48:04+00:00'],
    ['date', 'date', '2026-10-22'],
    ['jsonb', 'json', '{"count":1.5}'],
    ['custom_scalar', 'string', '123-custom'],
    ['text', 'text', '123'],
    ['boolean', 'boolean', 'false'],
    ['integer[]', 'array', '[1,2]'],
  ] as const)('preserves %s filters', (dataType, consoleDataType, value) => {
    const filter = { column: 'value', operator: '$eq', value };
    expect(
      transformFilterValues(
        [column(dataType, consoleDataType, dataType)],
        filter,
      ),
    ).toEqual(filter);
  });

  it.each([
    ['integer', 'integer', 'Int', '42', 42],
    ['int4', 'integer', 'Int', '-42', -42],
    ['float8', 'float', 'float8', '1.5', 1.5],
    ['double precision', 'float', 'float8', '-1.5', -1.5],
    ['numeric', 'number', 'numeric', '1.25', 1.25],
    ['real', 'float', 'Float', '1e-3', 0.001],
  ] as const)(
    'converts %s without truncating fractions',
    (dataType, consoleDataType, scalarType, value, expected) => {
      expect(
        transformFilterValues([column(dataType, consoleDataType, scalarType)], {
          column: 'value',
          operator: '$eq',
          value,
        }).value,
      ).toBe(expected);
    },
  );

  it.each([
    // Classified by the driver, whatever the scalar is called.
    ['INT64', 'integer', 'bigquery_int', '42', 42],
    ['FLOAT64', 'float', 'bigquery_float', '1.5', 1.5],
    ['custom_number', 'number', undefined, '2.5', 2.5],
    // Unclassified by the driver (GDC, or missing from the driver's map).
    ['tinyint', 'string', 'tinyint', '7', 7],
    ['smallmoney', 'string', 'smallmoney', '1.25', 1.25],
    ['number', 'string', 'number', '3.5', 3.5],
    ['int', 'string', 'Int', '8', 8],
  ] as const)(
    'converts %s columns across data sources',
    (dataType, consoleDataType, scalarType, value, expected) => {
      expect(
        transformFilterValues([column(dataType, consoleDataType, scalarType)], {
          column: 'value',
          operator: '$eq',
          value,
        }).value,
      ).toBe(expected);
    },
  );

  it.each([
    ['uuid', 'string', 'uuid'],
    ['uniqueidentifier', 'string', 'uniqueidentifier'],
    ['bit', 'integer', 'bit'],
    ['bit varying', 'integer', 'varbit'],
  ] as const)(
    'keeps %s filters as strings',
    (dataType, consoleDataType, scalarType) => {
      const filter = { column: 'value', operator: '$eq', value: '101' };
      expect(
        transformFilterValues(
          [column(dataType, consoleDataType, scalarType)],
          filter,
        ),
      ).toEqual(filter);
    },
  );

  it('keeps a uuid that starts with digits intact', () => {
    const filter = {
      column: 'value',
      operator: '$eq',
      value: '86d9aaba-f58b-4107-bfce-15e52f2d9fb8',
    };
    expect(
      transformFilterValues([column('uuid', 'uuid', 'uuid')], filter),
    ).toEqual(filter);
  });

  it('prefers the database column name over another column’s GraphQL name', () => {
    const renamedNumber = {
      ...column('integer', 'integer', 'Int'),
      name: 'id_number',
      graphQLProperties: { name: 'id', scalarType: 'Int' },
    };
    const uuidColumn = {
      ...column('uuid', 'uuid', 'uuid'),
      name: 'id',
      graphQLProperties: { name: 'uuid_id', scalarType: 'uuid' },
    };
    const filter = { column: 'id', operator: '$eq', value: '123' };
    expect(transformFilterValues([renamedNumber, uuidColumn], filter)).toEqual(
      filter,
    );
  });

  it('matches the database column name when the GraphQL field is renamed', () => {
    const renamedColumn = {
      ...column('float8', 'float'),
      graphQLProperties: { name: 'renamed', scalarType: 'float8' },
    };
    expect(
      transformFilterValues([renamedColumn], {
        column: 'value',
        operator: '$eq',
        value: '1.5',
      }).value,
    ).toBe(1.5);
  });

  it('works when GraphQL properties are absent and the data type is structured', () => {
    const { graphQLProperties: _, ...dbColumn } = column(
      { name: 'float8', type: 'float8' },
      'float',
    );
    expect(
      transformFilterValues([dbColumn], {
        column: 'value',
        operator: '$eq',
        value: '1.5',
      }).value,
    ).toBe(1.5);
  });

  it.each([
    '',
    ' ',
    'not a number',
    '1.5oops',
    'Infinity',
    '9007199254740993',
    '1.5',
  ])('does not corrupt invalid or unsafe integer input %j', (value) => {
    expect(
      transformFilterValues([column('integer', 'integer', 'Int')], {
        column: 'value',
        operator: '$eq',
        value,
      }).value,
    ).toBe(value);
  });

  it.each([null, undefined, false, 0, 1.5, [1, 2]].map((value) => [value]))(
    'preserves already typed values %j',
    (value) => {
      expect(
        transformFilterValues([column('integer', 'integer', 'Int')], {
          column: 'value',
          operator: '$eq',
          value,
        }).value,
      ).toEqual(value);
    },
  );

  it.each(['$is_null', '_is_null'])(
    'does not numerically coerce %s operands',
    (operator) => {
      const filter = { column: 'value', operator, value: 'true' };
      expect(
        transformFilterValues([column('integer', 'integer', 'Int')], filter),
      ).toEqual(filter);
    },
  );

  it('leaves an unknown column unchanged', () => {
    const filter = { column: 'unknown', operator: '$eq', value: '1.5' };
    expect(
      transformFilterValues([column('integer', 'integer')], filter),
    ).toEqual(filter);
  });

  it.each([
    ['uuid', 'uuid', '31b789b0-acfe-43a8-bdaf-561313e8f82e'],
    ['float8', 'float', '1.5'],
  ] as const)(
    'preserves the full %s value in the filter URL',
    (dataType, consoleDataType, value) => {
      const columns = [column(dataType, consoleDataType, dataType)];
      const filter = transformFilterValues(columns, {
        column: 'value',
        operator: '$eq',
        value,
      });
      const where = [{ value: { [filter.operator]: filter.value } }];
      const search = applyWhereAndSortConditionsToQueryString({
        options: { where },
        search: '',
      });
      expect(new URLSearchParams(search).get('filter')).toBe(
        `value;$eq;${value}`,
      );
      // URL reload must retain the same value, even for custom GraphQL scalars.
      expect(
        String(
          convertUrlToDataGridOptions(search, columns).where?.[0].value.$eq,
        ),
      ).toBe(value);
    },
  );
});
