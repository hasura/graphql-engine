export const SERVER_CONSOLE_MODE = 'server';

export const INTEGER = 'integer';
export const SERIAL = 'serial';
export const BIGINT = 'bigint';
export const BIGSERIAL = 'bigserial';
export const UUID = 'uuid';
export const JSONDTYPE = 'json';
export const JSONB = 'jsonb';
export const TIMESTAMP = 'timestamp with time zone';
export const TIME = 'time with time zone';
export const NUMERIC = 'numeric';
export const DATE = 'date';
export const TIMETZ = 'timetz';
export const BOOLEAN = 'boolean';
export const GRAPHQL_PATH = '/v1alpha1/graphql';

export const getPlaceholder = type => {
  switch (type) {
    case 'integer':
      return 'integer';
    case 'bigint':
      return 'BIG integer';
    case 'numeric':
      return 'float';
    case 'timestamp with time zone':
      return new Date().toISOString();
    case 'date':
      return new Date().toISOString().slice(0, 10);
    case 'timetz':
      const time = new Date().toISOString().slice(11, 19);
      return `${time}Z or ${time}+05:30`;
    case 'uuid':
      return 'UUID';
    case 'json':
      return '{"name": "foo"} or [12, "bar"]';
    case 'jsonb':
      return '{"name": "foo"} or [12, "bar"]';
    default:
      return 'text';
  }
};
