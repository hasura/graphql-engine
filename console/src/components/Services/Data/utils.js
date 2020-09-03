import {
  READ_ONLY_RUN_SQL_QUERIES,
  checkFeatureSupport,
} from '../../../helpers/versionUtils';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';
import { isJsonString } from '../../Common/utils/jsUtils';
import { ERROR_CODES } from './constants';
import { dataSource } from '../../../dataSources';

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

export const fetchTrackedTableListQuery = options => {
  const query = {
    type: 'select',
    args: {
      table: {
        name: 'hdb_table',
        schema: 'hdb_catalog',
      },
      columns: [
        'table_schema',
        'table_name',
        'is_enum',
        'configuration',
        {
          name: 'primary_key',
          columns: ['*'],
        },
        {
          name: 'relationships',
          columns: ['*'],
        },
        {
          name: 'permissions',
          columns: ['*'],
        },
        {
          name: 'unique_constraints',
          columns: ['*'],
        },
        {
          name: 'check_constraints',
          columns: ['*'],
          order_by: {
            column: 'constraint_name',
            type: 'asc',
          },
        },
        {
          name: 'computed_fields',
          columns: ['*'],
          order_by: {
            column: 'computed_field_name',
            type: 'asc',
          },
        },
      ],
      order_by: [{ column: 'table_name', type: 'asc' }],
    },
  };

  if (
    (options.schemas && options.schemas.length !== 0) ||
    (options.tables && options.tables.length !== 0)
  ) {
    query.where = {
      $or: [],
    };
  }
  if (options.schemas) {
    options.schemas.forEach(schemaName => {
      query.where.$or.push({
        table_schema: schemaName,
      });
    });
  }
  if (options.tables) {
    options.tables.forEach(tableInfo => {
      query.where.$or.push({
        table_schema: tableInfo.table_schema,
        table_name: tableInfo.table_name,
      });
    });
  }

  return query;
};

export const fetchTrackedTableFkQuery = options => {
  const runSql = dataSource.getFetchTrackedTableFkQuery(options);

  return getRunSqlQuery(
    runSql,
    false,
    checkFeatureSupport(READ_ONLY_RUN_SQL_QUERIES) ? true : false
  );
};

export const fetchTrackedTableReferencedFkQuery = options => {
  const runSql = dataSource.getFetchTrackedTableReferencedFkQuery(options);

  return getRunSqlQuery(
    runSql,
    false,
    checkFeatureSupport(READ_ONLY_RUN_SQL_QUERIES) ? true : false
  );
};

export const fetchTableListQuery = options => {
  const runSql = dataSource.getFetchTablesListQuery(options);

  return getRunSqlQuery(
    runSql,
    false,
    checkFeatureSupport(READ_ONLY_RUN_SQL_QUERIES) ? true : false
  );
};

// TODO -- each service should export this function and results should be "merged" to Table object
export const mergeLoadSchemaData = (
  infoSchemaTableData,
  hdbTableData,
  fkData,
  refFkData,
  metadataTables
) => {
  const _mergedTableData = [];

  infoSchemaTableData.forEach(infoSchemaTableInfo => {
    const _tableSchema = infoSchemaTableInfo.table_schema;
    const _tableName = infoSchemaTableInfo.table_name;
    const metadataTable = metadataTables?.find(
      t => t.table.schema === _tableSchema && t.table.name === _tableName
    );

    const trackedTableInfo = hdbTableData.find(
      t => t.table_schema === _tableSchema && t.table_name === _tableName
    );

    const _isTableTracked = trackedTableInfo ? true : false;

    const _columns = infoSchemaTableInfo.columns;
    const _comment = infoSchemaTableInfo.comment;
    const _tableType = infoSchemaTableInfo.table_type;
    const _triggers = infoSchemaTableInfo.triggers; // TODO: get from v1/query
    const _viewInfo = infoSchemaTableInfo.view_info; // TODO: get from v1/query

    let _primaryKey = null;
    let _relationships = [];
    let _permissions = [];
    let _uniqueConstraints = [];
    let _fkConstraints = [];
    let _refFkConstraints = [];
    let _remoteRelationships = [];
    let _isEnum = false;
    let _checkConstraints = [];
    let _configuration = {};
    let _computed_fields = [];

    if (_isTableTracked) {
      _primaryKey = trackedTableInfo.primary_key;
      _relationships = trackedTableInfo.relationships;
      _permissions = trackedTableInfo.permissions;
      _uniqueConstraints = trackedTableInfo.unique_constraints;
      _isEnum = trackedTableInfo.is_enum;
      _checkConstraints = trackedTableInfo.check_constraints;
      _configuration = trackedTableInfo.configuration;
      _computed_fields = trackedTableInfo.computed_fields;

      _fkConstraints = fkData.filter(
        fk => fk.table_schema === _tableSchema && fk.table_name === _tableName
      );

      _refFkConstraints = refFkData.filter(
        fk =>
          fk.ref_table_table_schema === _tableSchema &&
          fk.ref_table === _tableName
      );

      _remoteRelationships = (metadataTable.remote_relationships || []).map(
        ({ definition, name }) => ({
          remote_relationship_name: name,
          table_name: _tableName,
          table_schema: _tableSchema,
          definition,
        })
      );
    }

    const _mergedInfo = {
      table_schema: _tableSchema,
      table_name: _tableName,
      table_type: _tableType,
      is_table_tracked: _isTableTracked,
      columns: _columns,
      comment: _comment,
      triggers: _triggers,
      primary_key: _primaryKey,
      relationships: _relationships,
      permissions: _permissions,
      unique_constraints: _uniqueConstraints,
      check_constraints: _checkConstraints,
      foreign_key_constraints: _fkConstraints,
      opp_foreign_key_constraints: _refFkConstraints,
      view_info: _viewInfo,
      remote_relationships: _remoteRelationships,
      is_enum: _isEnum,
      configuration: _configuration,
      computed_fields: _computed_fields,
    };

    _mergedTableData.push(_mergedInfo);
  });

  return _mergedTableData;
};

export const cascadeUpQueries = (upQueries = [], isCascade = false) =>
  upQueries.map((i = {}) => {
    if (i.type === 'run_sql' || i.type === 'untrack_table') {
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
    err?.internal?.error?.status_code === dataSource.dependecyErrorCode
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
