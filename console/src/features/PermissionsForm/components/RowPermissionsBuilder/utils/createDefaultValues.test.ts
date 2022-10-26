import {
  schema,
  simpleExample,
  exampleWithBoolOperator,
  exampleWithRelationship,
  complicatedExample,
} from '../mocks';
import { createDefaultValues } from './createDefaultValues';

type Expected = {
  operators: Record<string, any> | undefined;
  filter: Record<string, any> | undefined;
};

test('renders basic permission', () => {
  const result = createDefaultValues({
    tableName: 'Album',
    schema,
    existingPermission: simpleExample,
  });

  const expected: Expected = {
    filter: {
      Title: {
        _eq: 'hello',
      },
    },
    operators: {
      filter: {
        columnOperator: '_eq',
        name: 'Title',
        type: 'column',
        typeName: 'Title',
      },
    },
  };

  expect(result).toEqual(expected);
});

test('renders bool operator permission', () => {
  const result = createDefaultValues({
    tableName: 'Album',
    schema,
    existingPermission: exampleWithBoolOperator,
  });

  const expected = {
    filter: {
      _and: [
        {
          age: {
            _eq: 8,
          },
        },
        {
          email: {
            _eq: 'adsff',
          },
        },
      ],
    },
    operators: {
      filter: {
        _and: ['age', 'email'],
        name: '_and',
        type: 'boolOperator',
        typeName: '_and',
      },
    },
  };

  expect(result).toEqual(expected);
});

test('renders permission with relationship', () => {
  const result = createDefaultValues({
    tableName: 'Album',
    schema,
    existingPermission: exampleWithRelationship,
  });

  const expected: Expected = {
    filter: {
      things: {
        name: {
          _eq: 'asdas',
        },
      },
    },
    operators: {
      filter: 'things',
    },
  };

  expect(result).toEqual(expected);
});

test('renders complex permission', () => {
  const result = createDefaultValues({
    tableName: 'user',
    schema,
    existingPermission: complicatedExample,
  });

  const expected: Expected = {
    filter: {
      _and: [
        {
          things: {
            _and: [
              {
                user: {
                  age: {
                    _eq: 22,
                  },
                },
              },
              {
                _or: [
                  {
                    fk_user_id: {
                      _eq: 'X-Hasura-User-Id',
                    },
                  },
                  {
                    user: {
                      age: {
                        _gte: 44,
                      },
                    },
                  },
                ],
              },
            ],
          },
        },
      ],
    },
    operators: {
      filter: {
        _and: [
          {
            name: 'things',
            things: {
              _and: [
                {
                  name: 'user',
                  type: 'relationship',
                  typeName: 'user',
                  user: {
                    columnOperator: '_eq',
                    name: 'age',
                    type: 'column',
                    typeName: 'age',
                  },
                },
                {
                  _or: [
                    {
                      columnOperator: '_eq',
                      name: 'fk_user_id',
                      type: 'column',
                      typeName: 'fk_user_id',
                    },
                    {
                      name: 'user',
                      type: 'relationship',
                      typeName: 'user',
                      user: {
                        columnOperator: '_gte',
                        name: 'age',
                        type: 'column',
                        typeName: 'age',
                      },
                    },
                  ],
                  name: '_or',
                  type: 'boolOperator',
                  typeName: '_or',
                },
              ],
              name: '_and',
              type: 'boolOperator',
              typeName: '_and',
            },
            type: 'relationship',
            typeName: 'thing',
          },
        ],
        name: '_and',
        type: 'boolOperator',
        typeName: '_and',
      },
    },
  };

  expect(result).toEqual(expected);
});
