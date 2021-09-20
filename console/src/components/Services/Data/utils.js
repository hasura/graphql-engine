import { isJsonString } from '../../Common/utils/jsUtils';
import { ERROR_CODES } from './constants';
import { dataSource } from '../../../dataSources';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';

export const getPlaceholder = type => {
  switch (type) {
    case dataSource.columnDataTypes.TIMESTAMP:
      return new Date().toISOString();
    case dataSource.columnDataTypes.DATE:
      return new Date().toISOString().slice(0, 10);
    case dataSource.columnDataTypes.TIME:
      const time = new Date().toISOString().slice(11, 19);
      return `${time}Z or ${time}+05:30`;
    case dataSource.columnDataTypes.JSONDTYPE:
      return '{"name": "foo"} or [12, "bar"]';
    case dataSource.columnDataTypes.JSONB:
      return '{"name": "foo"} or [12, "bar"]';
    case dataSource.columnDataTypes.BOOLEAN:
      return '';
    case dataSource.columnDataTypes.ARRAY:
      return '{"foo", "bar"} or ["foo", "bar"]';
    default:
      return type;
  }
};

export const tabNameMap = {
  browse: 'Browse Rows',
  insert: 'Insert Row',
  edit: 'Edit Row',
  modify: 'Modify',
  relationships: 'Relationships',
  permissions: 'Permissions',
};

export const ordinalColSort = (a, b) => {
  if (a.ordinal_position < b.ordinal_position) {
    return -1;
  }
  if (a.ordinal_position > b.ordinal_position) {
    return 1;
  }
  return 0;
};

export const getIngForm = string => {
  return (
    (string[string.length - 1] === 'e'
      ? string.slice(0, string.length - 1)
      : string) + 'ing'
  );
};

export const getEdForm = string => {
  return (
    (string[string.length - 1] === 'e'
      ? string.slice(0, string.length - 1)
      : string) + 'ed'
  );
};

export const escapeRegExp = string => {
  return string.replace(/([.*+?^${}()|[\]\\])/g, '\\$1');
};

export const getTableName = t => {
  const typ = typeof t;
  if (typ === 'string') {
    return t;
  } else if (typ === 'object') {
    return 'name' in t ? t.name : '';
  }
  return '';
};

export const fetchTrackedTableFkQuery = (options, source) => {
  const runSql = dataSource?.getFKRelations(options) || '';

  return getRunSqlQuery(runSql, source, false, true);
};

export const fetchTableListQuery = (options, source) => {
  const runSql = dataSource?.getFetchTablesListQuery(options) || '';

  return getRunSqlQuery(runSql, source, false, true);
};

// TODO: move to postgres service
const postgresFunctionTester = /.*\(\)$/gm;
export const isPostgresFunction = str =>
  new RegExp(postgresFunctionTester).test(str);
export const isTypeCast = (str = '') => str.split('::').length > 1;
export const quoteDefault = colDefault => {
  if (isPostgresFunction(colDefault) || isTypeCast(colDefault)) {
    return colDefault;
  }
  return `'${colDefault}'`;
};

export const cascadeUpQueries = (upQueries = [], isCascade = false) =>
  upQueries.map((i = {}) => {
    if (i.type.includes('run_sql') || i.type.includes('untrack_table')) {
      return {
        ...i,
        args: {
          ...i.args,
          ...(isCascade && { sql: dataSource.cascadeSqlQuery(i.args.sql) }),
          cascade: true,
        },
      };
    }
    return i;
  });

export const getDependencyError = (err = {}) => {
  if (err.code == ERROR_CODES.dependencyError.code) {
    // direct dependency error
    return { dependencyError: err };
  }
  if (err.code == ERROR_CODES.dataApiError.code) {
    // with CLI mode, error is getting as a string with the key `message`
    err = isJsonString(err.message) ? JSON.parse(err.message) : {};
  }

  if (err.code == ERROR_CODES.dependencyError.code)
    return {
      dependencyError: { ...err, message: err.error },
    };
  if (
    err.code === ERROR_CODES.postgresError.code &&
    err?.internal?.error?.status_code === dataSource.dependencyErrorCode
  )
    return {
      sqlDependencyError: {
        ...err,
        message: `${err?.internal?.error?.message}:\n
         ${err?.internal?.error?.description || ''}`,
      },
    };
  return {};
};

export const isInconsistentSource = (sourceName, inconsistentObjects) =>
  !!inconsistentObjects.find(i => sourceName === i.definition);

export const getSourceDriver = (dataSources, source) => {
  const sourceObject = dataSources.find(({ name }) => name === source);
  return sourceObject?.driver || sourceObject?.kind || 'postgres';
};
