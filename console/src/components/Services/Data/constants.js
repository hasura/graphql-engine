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

// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/index.html#error-codes
export const ERROR_CODES = {
  postgresError: {
    ERRORCODE: 'postgres-error',
    HTTPCODE: '500',
  },
  permissionDenied: {
    ERRORCODE: 'permission-denied',
    HTTPCODE: '400',
  },
  notExists: {
    ERRORCODE: 'not-exists',

    HTTPCODE: '400',
  },
  alreadyTracked: {
    ERRORCODE: 'already-tracked',
    HTTPCODE: '400',
  },
  accessDenied: {
    ERRORCODE: 'access-denied',

    HTTPCODE: '400',
  },
  notSupported: {
    ERRORCODE: 'not-supported',
    HTTPCODE: '400',
  },
  alreadyExists: {
    ERRORCODE: 'already-exists',
    HTTPCODE: '400',
  },
  invalidJson: {
    ERRORCODE: 'invalid-json',
    HTTPCODE: '400',
  },
  invalidHeaders: {
    ERRORCODE: 'invalid-headers',
    HTTPCODE: '400',
  },
  dependencyError: {
    ERRORCODE: 'dependency-error',
    HTTPCODE: '400',
  },
  parseFailed: {
    ERRORCODE: 'parse-failed',
    HTTPCODE: '400',
  },
  alreadyInitialised: {
    ERRORCODE: 'already-initialised',

    HTTPCODE: '400',
  },
  constraintError: {
    ERRORCODE: 'constraint-error',
    HTTPCODE: '400',
  },
  permissionError: {
    ERRORCODE: 'permission-error',

    HTTPCODE: '400',
  },
  unexpectedPayload: {
    ERRORCODE: 'unexpected-payload',
    HTTPCODE: '400',
  },
  invalidParams: {
    ERRORCODE: 'invalid-params',
    HTTPCODE: '400',
  },
  ' ': {
    ERRORCODE: ' ',
    description: ' <name> is relationship',
    HTTPCODE: '400',
  },
  unexpected: {
    ERRORCODE: 'unexpected',
    HTTPCODE: '500',
  },
  notFound: {
    ERRORCODE: 'not-found',
    ERRORS: ['No such resource exists'],
    HTTPCODE: '404',
  },
  // todo update this, this happens on bulk request failed
  dataApiError: {
    ERRORCODE: 'data_api_error',
  },
};
