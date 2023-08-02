import { tableColumns, sourceMetadata } from '../mocks';
import { createDefaultValues } from './createDefaultValues';

const tableName = 'bigquery_sample_sample_table';

type Expected = {
  operators: Record<string, any> | undefined;
  filter: Record<string, any> | undefined;
};

test('renders root value permission', () => {
  const result = createDefaultValues({
    tableName,
    tableColumns,
    sourceMetadata,
    existingPermission: { Series_reference: { _eq: 'X-Hasura-User-Id' } },
  });

  const expected: Expected = {
    operators: {
      filter: {
        name: 'Series_reference',
        typeName: 'Series_reference',
        type: 'column',
        columnOperator: '_eq',
      },
    },
    filter: { Series_reference: { _eq: 'X-Hasura-User-Id' } },
  };

  expect(result).toEqual(expected);
});

test('renders _exist permission', () => {
  const result = createDefaultValues({
    tableName,
    tableColumns,
    sourceMetadata,
    existingPermission: {
      _exists: {
        _table: { dataset: 'bigquery_sample', name: 'sample_table' },
        _where: { Data_value: { _eq: 'X-Hasura-User-Id' } },
      },
    },
  });

  const expected: Expected = {
    operators: { filter: '_exists' },
    filter: {
      _exists: {
        _table: { dataset: 'bigquery_sample', name: 'sample_table' },
        _where: { Data_value: { _eq: 'X-Hasura-User-Id' } },
      },
    },
  };

  expect(result).toEqual(expected);
});

test('renders _and operator permission', () => {
  const result = createDefaultValues({
    tableName,
    tableColumns,
    sourceMetadata,
    existingPermission: {
      _and: [
        { Data_value: { _eq: 'X-Hasura-User-Id' } },
        { Group: { _eq: 'X-Hasura-User-Id' } },
      ],
    },
  });

  const expected = {
    operators: {
      filter: {
        name: '_and',
        typeName: '_and',
        type: 'boolOperator',
        _and: [
          {
            name: 'Data_value',
            typeName: 'Data_value',
            type: 'column',
            columnOperator: '_eq',
          },
          'Group',
        ],
      },
    },
    filter: {
      _and: [
        { Data_value: { _eq: 'X-Hasura-User-Id' } },
        { Group: { _eq: 'X-Hasura-User-Id' } },
      ],
    },
  };
  expect(result).toEqual(expected);
});

test('renders _not operator with number value permission', () => {
  const result = createDefaultValues({
    tableName,
    tableColumns,
    sourceMetadata,
    existingPermission: { _not: { Data_value: { _eq: 1337 } } },
  });

  const expected = {
    operators: {
      filter: {
        name: '_not',
        typeName: '_not',
        type: 'boolOperator',
        _not: {
          name: 'Data_value',
          typeName: 'Data_value',
          type: 'column',
          columnOperator: '_eq',
        },
      },
    },
    filter: { _not: { Data_value: { _eq: 1337 } } },
  };

  expect(result).toEqual(expected);
});
