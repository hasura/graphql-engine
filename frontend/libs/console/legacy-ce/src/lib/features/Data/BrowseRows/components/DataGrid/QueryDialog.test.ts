import { describe, expect, it } from 'vitest';
import { TableColumn } from '@hasura/metadata/data-source';
import { FilterClause, transformFilterValues } from './QueryDialog';

const column = (
  name: string,
  consoleDataType: TableColumn['consoleDataType'],
  scalarType?: string,
): TableColumn => ({
  name,
  dataType: consoleDataType,
  consoleDataType,
  graphQLProperties: { name, scalarType },
});

const columns: TableColumn[] = [
  column('id', 'uuid', 'uuid'),
  column('created_at', 'timestamp', 'timestamptz'),
  column('price', 'float', 'float8'),
  column('quantity', 'integer', 'Int'),
  column('payload', 'json', 'jsonb'),
  column('name', 'text', 'String'),
  column('is_active', 'boolean', 'Boolean'),
];

const filter = (columnName: string, value: unknown): FilterClause => ({
  column: columnName,
  operator: '$eq',
  value,
});

describe('transformFilterValues', () => {
  it('keeps a uuid value intact instead of truncating it to its numeric prefix', () => {
    const uuid = '31b789b0-acfe-43a8-bdaf-561313e8f82e';

    expect(transformFilterValues(columns, filter('id', uuid))).toEqual({
      column: 'id',
      operator: '$eq',
      value: uuid,
    });
  });

  it('keeps a timestamptz value intact', () => {
    const timestamp = '2026-10-21T06:58:34Z';

    expect(
      transformFilterValues(columns, filter('created_at', timestamp)),
    ).toEqual({
      column: 'created_at',
      operator: '$eq',
      value: timestamp,
    });
  });

  it('keeps a json value intact', () => {
    expect(
      transformFilterValues(columns, filter('payload', '{"a":1}')),
    ).toEqual({
      column: 'payload',
      operator: '$eq',
      value: '{"a":1}',
    });
  });

  it('keeps text and boolean values untouched', () => {
    expect(transformFilterValues(columns, filter('name', 'AC/DC'))).toEqual({
      column: 'name',
      operator: '$eq',
      value: 'AC/DC',
    });
    expect(transformFilterValues(columns, filter('is_active', 'true'))).toEqual(
      {
        column: 'is_active',
        operator: '$eq',
        value: 'true',
      },
    );
  });

  it('coerces an integer column to a number', () => {
    expect(transformFilterValues(columns, filter('quantity', '42'))).toEqual({
      column: 'quantity',
      operator: '$eq',
      value: 42,
    });
  });

  it('preserves the fractional part of a float column', () => {
    expect(transformFilterValues(columns, filter('price', '1.5'))).toEqual({
      column: 'price',
      operator: '$eq',
      value: 1.5,
    });
  });

  it('leaves an empty numeric value unset rather than coercing it to 0', () => {
    expect(transformFilterValues(columns, filter('quantity', ''))).toEqual({
      column: 'quantity',
      operator: '$eq',
      value: '',
    });
  });

  it('returns the filter untouched when the column is unknown', () => {
    const unknown = filter('does_not_exist', 'x');

    expect(transformFilterValues(columns, unknown)).toBe(unknown);
  });
});
