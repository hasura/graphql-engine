export const Operators = [
  { name: 'equals', value: '$eq' },
  { name: 'not equals', value: '$ne' },
  { name: 'in', value: '$in' },
  { name: 'not in', value: '$nin' },
  { name: '>', value: '$gt' },
  { name: '<', value: '$lt' },
  { name: '>=', value: '$gte' },
  { name: '<=', value: '$lte' },
  { name: 'like', value: '$like' },
  { name: 'not like', value: '$nlike' },
  { name: 'ilike', value: '$ilike' },
  { name: 'not ilike', value: '$nilike' },
  { name: 'similar', value: '$similar' },
  { name: 'not similar', value: '$nsimilar' },
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
