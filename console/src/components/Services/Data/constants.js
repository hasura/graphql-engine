export const Operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: 'in', value: '$in', graphqlOp: '_in' },
  { name: 'not in', value: '$nin', graphqlOp: '_nin' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
  { name: 'like', value: '$like', graphqlOp: '_like' },
  { name: 'not like', value: '$nlike', graphqlOp: '_nlike' },
  { name: 'like (case-insensitive)', value: '$ilike', graphqlOp: '_ilike' },
  {
    name: 'not like (case-insensitive)',
    value: '$nilike',
    graphqlOp: '_nilike',
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

export const Reals = ['float4', 'float8', 'numeric'];

export const Numerics = [...Integers, ...Reals];

export const defaultDataTypeToCast = 'text';
