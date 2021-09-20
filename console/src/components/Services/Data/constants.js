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

  {
    name: '~',
    value: '$regex',
    graphqlOp: '_regex',
  },
  {
    name: '~*',
    value: '$iregex',
    graphqlOp: '_iregex',
  },
  {
    name: '!~',
    value: '$nregex',
    graphqlOp: '_nregex',
  },
  {
    name: '!~*',
    value: '$niregex',
    graphqlOp: '_niregex',
  },
];

export const Integers = [
  'serial',
  'integer',
  'bigserial',
  'smallint',
  'bigint',
];

export const COUNT_LIMIT = 100000;

export const maxAllowedColumnLength = 64;

const maxAllowedLength = 255;
const unixEpochLength = 14;
export const maxAllowedMigrationLength = maxAllowedLength - unixEpochLength;

export const Reals = ['float4', 'float8', 'numeric'];

export const Numerics = [...Integers, ...Reals];

export const defaultDataTypeToCast = 'text';

// https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/index.html#error-codes
export const ERROR_CODES = {
  postgresError: {
    code: 'postgres-error',
    httpCode: '500',
  },
  permissionDenied: {
    code: 'permission-denied',
    httpCode: '400',
  },
  notExists: {
    code: 'not-exists',

    httpCode: '400',
  },
  alreadyTracked: {
    code: 'already-tracked',
    httpCode: '400',
  },
  accessDenied: {
    code: 'access-denied',

    httpCode: '400',
  },
  notSupported: {
    code: 'not-supported',
    httpCode: '400',
  },
  alreadyExists: {
    code: 'already-exists',
    httpCode: '400',
  },
  invalidJson: {
    code: 'invalid-json',
    httpCode: '400',
  },
  invalidHeaders: {
    code: 'invalid-headers',
    httpCode: '400',
  },
  dependencyError: {
    code: 'dependency-error',
    httpCode: '400',
  },
  parseFailed: {
    code: 'parse-failed',
    httpCode: '400',
  },
  alreadyInitialised: {
    code: 'already-initialised',

    httpCode: '400',
  },
  constraintError: {
    code: 'constraint-error',
    httpCode: '400',
  },
  permissionError: {
    code: 'permission-error',

    httpCode: '400',
  },
  unexpectedPayload: {
    code: 'unexpected-payload',
    httpCode: '400',
  },
  invalidParams: {
    code: 'invalid-params',
    httpCode: '400',
  },
  ' ': {
    code: ' ',
    description: ' <name> is relationship',
    httpCode: '400',
  },
  unexpected: {
    code: 'unexpected',
    httpCode: '500',
  },
  notFound: {
    code: 'not-found',
    ERRORS: ['No such resource exists'],
    httpCode: '404',
  },
  dataApiError: {
    code: 'data_api_error',
  },
};
