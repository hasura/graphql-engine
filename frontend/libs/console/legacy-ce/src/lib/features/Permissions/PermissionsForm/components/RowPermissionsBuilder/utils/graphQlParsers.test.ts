import {
  getAllColumnsAndOperators,
  getColumnOperators,
} from './graphqlParsers';
import { schema } from '../mocks';

test('correctly fetches items for dropdown from schema', () => {
  const result = getAllColumnsAndOperators({
    tableName: 'user',
    schema,
    tableConfig: {},
  });

  expect(result.boolOperators.length).toBe(3);
  expect(result.columns.length).toBe(4);
  expect(result.relationships.length).toBe(1);
  expect(result.columns.map(col => col.name)).toEqual([
    'age',
    'email',
    'id',
    'name',
  ]);
  expect(result.relationships[0].name).toBe('things');
});

test('correctly fetches operators for a given column', () => {
  const result = getColumnOperators({
    tableName: 'user',
    schema,
    columnName: 'age',
    tableConfig: {},
  });

  const expected: ReturnType<typeof getColumnOperators> = [
    {
      name: '_eq',
      type: {
        type: 'float8',
      },
    },
    {
      name: '_gt',
      type: {
        type: 'float8',
      },
    },
    {
      name: '_gte',
      type: {
        type: 'float8',
      },
    },
    {
      name: '_in',
      type: {
        isList: true,
        type: 'float8',
      },
    },
    {
      name: '_is_null',
      type: {
        type: 'Boolean',
      },
    },
    {
      name: '_lt',
      type: {
        type: 'float8',
      },
    },
    {
      name: '_lte',
      type: {
        type: 'float8',
      },
    },
    {
      name: '_neq',
      type: {
        type: 'float8',
      },
    },
    {
      name: '_nin',
      type: {
        isList: true,
        type: 'float8',
      },
    },
  ];

  expect(result).toEqual(expected);
});
