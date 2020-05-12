export const Operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: 'in', value: '$in', graphqlOp: '_in', defaultValue: '[]' },
  { name: 'not in', value: '$nin', graphqlOp: '_nin', defaultValue: '[]' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
  { name: 'like', value: '$like', graphqlOp: '_like', defaultValue: '%%' },
  {
    name: 'not like',
    value: '$nlike',
    graphqlOp: '_nlike',
    defaultValue: '%%',
  },
  {
    name: 'like (case-insensitive)',
    value: '$ilike',
    graphqlOp: '_ilike',
    defaultValue: '%%',
  },
  {
    name: 'not like (case-insensitive)',
    value: '$nilike',
    graphqlOp: '_nilike',
    defaultValue: '%%',
  },
  { name: 'similar', value: '$similar', graphqlOp: '_similar' },
  { name: 'not similar', value: '$nsimilar', graphqlOp: '_nsimilar' },
];

export const Integers = [
  'serial',
  'integer',
  'bigserial',
  'smallint',
  'bigint',
];

export const COUNT_LIMIT = 100000;

export const Reals = ['float4', 'float8', 'numeric'];

export const Numerics = [...Integers, ...Reals];

export const defaultDataTypeToCast = 'text';
