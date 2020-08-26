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

const findFKConstraint = (curTable, column) => {
  const fkConstraints = curTable.foreign_key_constraints;
  return fkConstraints.find(
    fk =>
      Object.keys(fk.column_mapping).length === column.length &&
      Object.keys(fk.column_mapping).join(',') === column.join(',')
  );
};

const findOppFKConstraint = (curTable, column) => {
  const fkConstraints = curTable.opp_foreign_key_constraints;
  return fkConstraints.find(
    fk =>
      Object.keys(fk.column_mapping).length === column.length &&
      Object.keys(fk.column_mapping).join(',') === column.join(',')
  );
};

export const findTableFromRel = (schemas, curTable, rel) => {
  let rTable = null;
  let rSchema = 'public';

  // for view
  if (rel.rel_def.manual_configuration !== undefined) {
    rTable = rel.rel_def.manual_configuration.remote_table;
    if (rTable.schema) {
      rSchema = rTable.schema;
      rTable = rTable.name;
    }
  }

  // for table
  if (rel.rel_def.foreign_key_constraint_on !== undefined) {
    // for object relationship
    if (rel.rel_type === 'object') {
      const column = [rel.rel_def.foreign_key_constraint_on];
      const fkc = findFKConstraint(curTable, column);
      if (fkc) {
        rTable = fkc.ref_table;
        rSchema = fkc.ref_table_table_schema;
      }
    }

    // for array relationship
    if (rel.rel_type === 'array') {
      rTable = rel.rel_def.foreign_key_constraint_on.table;
      if (rTable.schema) {
        rSchema = rTable.schema;
        rTable = rTable.name;
      }
    }
  }
  return schemas.find(
    x => x.table_name === rTable && x.table_schema === rSchema
  );
};

export const findAllFromRel = (schemas, curTable, rel) => {
  const relMeta = {
    relName: rel.rel_name,
    lTable: rel.table_name,
    lSchema: rel.table_schema,
    isObjRel: rel.rel_type === 'object',
    lcol: null,
    rcol: null,
    rTable: null,
    rSchema: null,
  };

  // for view
  if (rel.rel_def.manual_configuration !== undefined) {
    const rTableConfig = rel.rel_def.manual_configuration.remote_table;
    if (rTableConfig.schema) {
      relMeta.rTable = rTableConfig.name;
      relMeta.rSchema = rTableConfig.schema;
    } else {
      relMeta.rTable = rTableConfig;
      relMeta.rSchema = 'public';
    }
    const columnMapping = rel.rel_def.manual_configuration.column_mapping;
    relMeta.lcol = Object.keys(columnMapping);
    relMeta.rcol = relMeta.lcol.map(column => columnMapping[column]);
  }

  // for table
  const foreignKeyConstraintOn = rel.rel_def.foreign_key_constraint_on;
  if (foreignKeyConstraintOn !== undefined) {
    // for object relationship
    if (rel.rel_type === 'object') {
      relMeta.lcol = [foreignKeyConstraintOn];
      const fkc = findFKConstraint(curTable, relMeta.lcol);
      if (fkc) {
        relMeta.rTable = fkc.ref_table;
        relMeta.rSchema = fkc.ref_table_table_schema;
        relMeta.rcol = [fkc.column_mapping[relMeta.lcol]];
      }
    }

    // for array relationship
    if (rel.rel_type === 'array') {
      relMeta.rcol = [foreignKeyConstraintOn.column];
      const rTableConfig = foreignKeyConstraintOn.table;
      if (rTableConfig.schema) {
        relMeta.rTable = rTableConfig.name;
        relMeta.rSchema = rTableConfig.schema;
      } else {
        relMeta.rTable = rTableConfig;
        relMeta.rSchema = 'public';
      }
      const rfkc = findOppFKConstraint(curTable, relMeta.rcol);
      relMeta.lcol = [rfkc.column_mapping[relMeta.rcol]];
    }
  }
  return relMeta;
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

const generateWhereObject = options => {
  const where = {};
  if (options.schemas) {
    options.schemas.forEach(s => {
      if (!where.$and) where.$and = [];

      where.$and.push({
        table_schema: s,
      });
    });
  }
  if (options.tables) {
    options.tables.forEach(t => {
      if (!where.$and) where.$and = [];
      where.$and.push({
        table_schema: t.table_schema,
        table_name: t.table_name,
      });
    });
  }
  return where;
};

export const fetchTrackedTableRemoteRelationshipQuery = options => {
  const query = {
    type: 'select',
    args: {
      table: {
        schema: 'hdb_catalog',
        name: 'hdb_remote_relationship',
      },
      columns: ['*.*', 'remote_relationship_name'],
      where: generateWhereObject(options),
      order_by: [{ column: 'remote_relationship_name', type: 'asc' }],
    },
  };
  return query;
};

export const mergeLoadSchemaData = (
  infoSchemaTableData,
  hdbTableData,
  fkData,
  refFkData,
  remoteRelData
) => {
  const _mergedTableData = [];

  infoSchemaTableData.forEach(infoSchemaTableInfo => {
    const _tableSchema = infoSchemaTableInfo.table_schema;
    const _tableName = infoSchemaTableInfo.table_name;

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

      _remoteRelationships = remoteRelData.filter(
        rel =>
          rel.table_schema === _tableSchema && rel.table_name === _tableName
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

export const commonDataTypes = [
  {
    name: 'Integer',
    value: 'integer',
    description: 'signed four-byte integer',
    hasuraDatatype: 'integer',
  },
  {
    name: 'Integer (auto-increment)',
    value: 'serial',
    description: 'autoincrementing four-byte integer',
    hasuraDatatype: null,
  },
  {
    name: 'Text',
    value: 'text',
    description: 'variable-length character string',
    hasuraDatatype: 'text',
  },
  {
    name: 'Boolean',
    value: 'boolean',
    description: 'logical Boolean (true/false)',
    hasuraDatatype: 'boolean',
  },
  {
    name: 'Numeric',
    value: 'numeric',
    description: 'exact numeric of selected precision',
    hasuraDatatype: 'numeric',
  },
  {
    name: 'Timestamp',
    value: 'timestamptz',
    description: 'date and time, including time zone',
    hasuraDatatype: 'timestamp with time zone',
  },
  {
    name: 'Time',
    value: 'timetz',
    description: 'time of day (no time zone)',
    hasuraDatatype: 'time with time zone',
  },
  {
    name: 'Date',
    value: 'date',
    description: 'calendar date (year, month, day)',
    hasuraDatatype: 'date',
  },
  {
    name: 'UUID',
    value: 'uuid',
    description: 'universal unique identifier',
    hasuraDatatype: 'uuid',
  },
  {
    name: 'JSONB',
    value: 'jsonb',
    description: 'binary format JSON data',
    hasuraDatatype: 'jsonb',
  },
  {
    name: 'Big Integer',
    value: 'bigint',
    description: 'signed eight-byte integer',
    hasuraDatatype: 'bigint',
  },
  {
    name: 'Big Integer (auto-increment)',
    value: 'bigserial',
    description: 'autoincrementing eight-byte integer',
    hasuraDatatype: null,
  },
];

/*
 * Fetch non-composite types, primitive types like text, varchar etc,
 * Filter types whose typename is unknown and type category is not 'Pseudo' and it is valid and available to be used
 * */
export const fetchColumnTypesQuery = `
SELECT
  string_agg(t.typname, ',') as "Type Name",
  string_agg(pg_catalog.format_type(t.oid, NULL), ',') as "Display Name",
  string_agg(coalesce(pg_catalog.obj_description(t.oid, 'pg_type'), ''), ':') as "Descriptions",
  t.typcategory
FROM pg_catalog.pg_type t
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid))
  AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)
  AND pg_catalog.pg_type_is_visible(t.oid)
  AND t.typname != 'unknown'
  AND t.typcategory != 'P'
GROUP BY t.typcategory;`;

export const fetchColumnDefaultFunctions = (schema = 'public') => `
SELECT string_agg(pgp.proname, ','),
  t.typname as "Type"
from pg_proc pgp
JOIN pg_type t
ON pgp.prorettype = t.oid
JOIN pg_namespace pgn
ON pgn.oid = pgp.pronamespace
WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid))
  AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)
  AND pg_catalog.pg_type_is_visible(t.oid)
  AND t.typname != 'unknown'
  AND t.typcategory != 'P'
  AND (array_length(pgp.proargtypes, 1) = 0)
  AND ( pgn.nspname = '${schema}' OR pgn.nspname = 'pg_catalog' )
  AND pgp.proretset=false
GROUP BY t.typname
ORDER BY t.typname ASC;
`;

const postgresFunctionTester = /.*\(\)$/gm;

export const isPostgresFunction = str =>
  new RegExp(postgresFunctionTester).test(str);

export const getEstimateCountQuery = (schemaName, tableName) => {
  return `
SELECT
  reltuples::BIGINT
FROM
  pg_class
WHERE
  oid = (quote_ident('${schemaName}') || '.' || quote_ident('${tableName}'))::regclass::oid
  AND relname = '${tableName}';
`;
};

export const isColTypeString = colType =>
  ['text', 'varchar', 'char', 'bpchar', 'name'].includes(colType);

const cascadePGSqlQuery = sql => {
  if (sql[sql.length - 1] === ';')
    return sql.substr(0, sql.length - 1) + ' CASCADE;';
  // SQL might have  a " at the end
  else if (sql[sql.length - 2] === ';')
    return sql.substr(0, sql.length - 2) + ' CASCADE;';
  return sql + ' CASCADE;';
};

export const cascadeUpQueries = (upQueries = [], isPgCascade = false) =>
  upQueries.map((i = {}) => {
    if (i.type === 'run_sql' || i.type === 'untrack_table') {
      return {
        ...i,
        args: {
          ...i.args,
          ...(isPgCascade && { sql: cascadePGSqlQuery(i.args.sql) }),
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
    err?.internal?.error?.status_code === '2BP01' // pg dependent error > https://www.postgresql.org/docs/current/errcodes-appendix.html
  )
    return {
      pgDependencyError: {
        ...err,
        message: `${err?.internal?.error?.message}:\n
         ${err?.internal?.error?.description || ''}`,
      },
    };
  return {};
};
