import React from 'react';
import { DataSourcesAPI } from '../..';
import { QualifiedTable } from '../../../metadata/types';
import {
  TableColumn,
  Table,
  BaseTableColumn,
  SupportedFeaturesType,
  ViolationActions,
} from '../../types';
import { generateTableRowRequest, operators } from './utils';

const permissionColumnDataTypes = {
  character: [
    'char',
    'varchar',
    'text',
    'nchar',
    'nvarchar',
    'binary',
    'vbinary',
    'image',
  ],
  numeric: [
    'bit',
    'tinyint',
    'smallint',
    'int',
    'bigint',
    'decimal',
    'numeric',
    'smallmoney',
    'money',
    'float',
    'real',
  ],
  dateTime: [
    'datetime',
    'smalldatetime',
    'date',
    'time',
    'datetimeoffset',
    'timestamp',
  ],
  user_defined: [],
};

const supportedColumnOperators = [
  '_is_null',
  '_eq',
  '_neq',
  '_gt',
  '_lt',
  '_gte',
  '_lte',
];

const isTable = (table: Table) => {
  if (!table.table_type) return true; // todo
  return table.table_type === 'TABLE' || table.table_type === 'BASE TABLE';
};

export const isColTypeString = (colType: string) =>
  ['text', 'varchar', 'char', 'bpchar', 'name'].includes(colType);

const columnDataTypes = {
  INTEGER: 'integer',
  BIGINT: 'bigint',
  GUID: 'guid',
  JSONDTYPE: 'nvarchar',
  DATETIMEOFFSET: 'timestamp with time zone',
  NUMERIC: 'numeric',
  DATE: 'date',
  TIME: 'time',
  TEXT: 'text',
};

// eslint-disable-next-line no-useless-escape
const createSQLRegex = /create\s*(?:|or\s*replace)\s*(?<type>view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((?<schema>\"?\w+\"?)\.(?<tableWithSchema>\"?\w+\"?)|(?<table>\"?\w+\"?))\s*(?<partition>partition\s*of)?/gim;

export const displayTableName = (table: Table) => {
  const tableName = table.table_name;

  return isTable(table) ? <span>{tableName}</span> : <i>{tableName}</i>;
};

const violationActions: ViolationActions[] = [
  'no action',
  'cascade',
  'set null',
  'set default',
];

export const supportedFeatures: SupportedFeaturesType = {
  driver: {
    name: 'mssql',
  },
  schemas: {
    create: {
      enabled: true,
    },
    delete: {
      enabled: true,
    },
  },
  tables: {
    create: {
      enabled: true,
      frequentlyUsedColumns: false,
    },
    browse: {
      enabled: true,
      customPagination: true,
      aggregation: false,
    },
    insert: {
      enabled: false,
    },
    modify: {
      enabled: true,
      readOnly: true,
      columns: {
        view: true,
        edit: false,
      },
      computedFields: false,
      primaryKeys: {
        view: true,
        edit: false,
      },
      foreignKeys: {
        view: true,
        edit: false,
      },
      uniqueKeys: {
        view: true,
        edit: false,
      },
      triggers: false,
      checkConstraints: {
        view: true,
        edit: false,
      },
      customGqlRoot: false,
      setAsEnum: false,
      untrack: true,
      delete: false,
      comments: {
        view: true,
        edit: false,
      },
    },
    relationships: {
      enabled: true,
      track: true,
    },
    permissions: {
      enabled: true,
    },
    track: {
      enabled: false,
    },
  },
  functions: {
    enabled: true,
    track: {
      enabled: false,
    },
    nonTrackableFunctions: {
      enabled: false,
    },
  },
  events: {
    triggers: {
      enabled: true,
      add: false,
    },
  },
  actions: {
    enabled: true,
    relationships: false,
  },
  rawSQL: {
    enabled: true,
    statementTimeout: false,
    tracking: true,
  },
  connectDbForm: {
    enabled: true,
    connectionParameters: false,
    databaseURL: true,
    environmentVariable: true,
    read_replicas: false,
    prepared_statements: false,
    isolation_level: false,
    connectionSettings: true,
    retries: false,
    pool_timeout: false,
    connection_lifetime: false,
    ssl_certificates: false,
  },
};

export const isJsonColumn = (column: BaseTableColumn): boolean => {
  return column.data_type_name === 'json' || column.data_type_name === 'jsonb';
};

const defaultRedirectSchema = 'dbo';

export const mssql: DataSourcesAPI = {
  isTable,
  isJsonColumn,
  displayTableName,
  operators,
  generateTableRowRequest,
  getFunctionSchema: () => {
    return '';
  },
  getFunctionDefinition: () => {
    return '';
  },
  getSchemaFunctions: () => {
    return [];
  },
  findFunction: () => {
    return undefined;
  },
  getGroupedTableComputedFields: () => {
    return { scalar: [], table: [] };
  },
  isColumnAutoIncrement: () => {
    return false;
  },
  getTableSupportedQueries: () => {
    // since only subscriptions and queries are supported on MSSQL atm.
    return ['select'];
  },
  getColumnType: (col: TableColumn) => col.data_type_name ?? col.data_type,
  arrayToPostgresArray: () => {
    return '';
  },
  schemaListSql: () => `
SELECT
  s.name AS schema_name
FROM
  sys.schemas s
WHERE
  s.name NOT IN (
    'guest', 'INFORMATION_SCHEMA', 'sys',
    'db_owner', 'db_securityadmin', 'db_accessadmin',
    'db_backupoperator', 'db_ddladmin', 'db_datawriter',
    'db_datareader', 'db_denydatawriter', 'db_denydatareader'
  )
ORDER BY
  s.name
`,
  parseColumnsInfoResult: () => {
    return {};
  },
  columnDataTypes,
  getFetchTablesListQuery: ({ schemas, tables }) => {
    let whereClause = '';

    if (schemas) {
      whereClause = `AND sch.name IN (${schemas.map(s => `'${s}'`).join(',')})`;
    } else if (tables) {
      whereClause = `AND obj.name IN (${tables
        .map(t => `'${t.table_name}'`)
        .join(',')})`;
    }

    return `
  SELECT sch.name as table_schema,
    obj.name as table_name,
    case
        when obj.type = 'AF' then 'Aggregate function (CLR)'
        when obj.type = 'C' then 'CHECK constraint'
        when obj.type = 'D' then 'DEFAULT (constraint or stand-alone)'
        when obj.type = 'F' then 'FOREIGN KEY constraint'
        when obj.type = 'FN' then 'SQL scalar function'
        when obj.type = 'FS' then 'Assembly (CLR) scalar-function'
        when obj.type = 'FT' then 'Assembly (CLR) table-valued function'
        when obj.type = 'IF' then 'SQL inline table-valued function'
        when obj.type = 'IT' then 'Internal table'
        when obj.type = 'P' then 'SQL Stored Procedure'
        when obj.type = 'PC' then 'Assembly (CLR) stored-procedure'
        when obj.type = 'PG' then 'Plan guide'
        when obj.type = 'PK' then 'PRIMARY KEY constraint'
        when obj.type = 'R' then 'Rule (old-style, stand-alone)'
        when obj.type = 'RF' then 'Replication-filter-procedure'
        when obj.type = 'S' then 'System base table'
        when obj.type = 'SN' then 'Synonym'
        when obj.type = 'SO' then 'Sequence object'
        when obj.type = 'U' then 'TABLE'
        when obj.type = 'EC' then 'Edge constraint'
        when obj.type = 'V' then 'VIEW'
    end as table_type,
    (SELECT e.[value] AS comment for json path),
    JSON_QUERY([isc].json) AS columns
FROM sys.objects as obj
    INNER JOIN sys.schemas as sch ON obj.schema_id = sch.schema_id
    LEFT JOIN sys.extended_properties AS e ON major_id = obj.object_id
    OUTER APPLY (
        SELECT
            a.name AS column_name,
            a.column_id AS ordinal_position,
            ad.definition AS column_default,
            a.collation_name AS collation_name,
            CASE
                WHEN a.is_nullable = 0
                OR t.is_nullable = 0
                THEN 'NO'
                ELSE 'YES'
            END AS is_nullable,
            CASE
                WHEN t.is_table_type = 1 THEN 'TABLE'
                WHEN t.is_assembly_type = 1 THEN 'ASSEMBLY'
                WHEN t.is_user_defined = 1 THEN 'USER-DEFINED'
                ELSE 'OTHER'
            END AS data_type,
            t.name AS data_type_name
        FROM
            sys.columns a
            LEFT JOIN sys.default_constraints ad ON (a.column_id = ad.parent_column_id AND a.object_id = ad.parent_object_id)
            JOIN sys.types t ON a.user_type_id = t.user_type_id
        WHERE a.column_id > 0 and a.object_id = obj.object_id
        FOR JSON path
) AS [isc](json) where obj.type_desc in ('USER_TABLE', 'VIEW') ${whereClause};`;
  },
  commonDataTypes: [],
  fetchColumnTypesQuery: '',
  fetchColumnDefaultFunctions: () => {
    return '';
  },
  isSQLFunction: () => {
    return false;
  },
  getEstimateCountQuery: () => {
    return '';
  },
  isColTypeString: () => {
    return false;
  },
  cascadeSqlQuery: () => {
    return '';
  },
  dependencyErrorCode: '',
  getCreateTableQueries: (
    currentSchema: string,
    tableName: string,
    columns: any[],
    primaryKeys: (number | string)[],
    foreignKeys: any[],
    uniqueKeys: any[],
    checkConstraints: any[],
    tableComment?: string | undefined
  ) => {
    const currentCols = columns.filter(c => c.name !== '');
    const flatUniqueKeys = uniqueKeys.reduce((acc, val) => acc.concat(val), []);

    const pKeys = primaryKeys
      .filter(p => p !== '')
      .map(p => currentCols[p as number].name);

    let tableDefSql = '';
    for (let i = 0; i < currentCols.length; i++) {
      tableDefSql += `${currentCols[i].name} ${currentCols[i].type}`;

      // check if column is nullable
      if (!currentCols[i].nullable) {
        tableDefSql += ' NOT NULL';
      }

      // check if the column is unique
      if (uniqueKeys.length && flatUniqueKeys.includes(i)) {
        tableDefSql += ' UNIQUE';
      }

      // check if column has a default value
      if (
        currentCols[i].default !== undefined &&
        currentCols[i].default?.value !== ''
      ) {
        if (isColTypeString(currentCols[i].type)) {
          // if a column type is text and if it has a non-func default value, add a single quote
          tableDefSql += ` DEFAULT '${currentCols[i]?.default?.value}'`;
        } else {
          tableDefSql += ` DEFAULT ${currentCols[i]?.default?.value}`;
        }
      }

      tableDefSql += i === currentCols.length - 1 ? '' : ', ';
    }

    // add primary key
    if (pKeys.length > 0) {
      tableDefSql += ', PRIMARY KEY (';
      tableDefSql += pKeys.map(col => `${col}`).join(',');
      tableDefSql += ') ';
    }

    const numFks = foreignKeys.length;
    let fkDupColumn = null;
    if (numFks > 1) {
      foreignKeys.forEach((fk, _i) => {
        if (_i === numFks - 1) {
          return;
        }

        const { colMappings, refTableName, onUpdate, onDelete } = fk;

        const mappingObj: Record<string, string> = {};
        const rCols: string[] = [];
        const lCols: string[] = [];

        colMappings
          .slice(0, -1)
          .forEach((cm: { column: string | number; refColumn: string }) => {
            if (mappingObj[cm.column] !== undefined) {
              fkDupColumn = columns[cm.column as number].name;
            }

            mappingObj[cm.column] = cm.refColumn;
            lCols.push(`"${columns[cm.column as number].name}"`);
            rCols.push(`"${cm.refColumn}"`);
          });

        if (lCols.length === 0) {
          return;
        }
        tableDefSql += `, FOREIGN KEY (${lCols.join(', ')}) REFERENCES "${
          fk.refSchemaName
        }"."${refTableName}"(${rCols.join(
          ', '
        )}) ON UPDATE ${onUpdate} ON DELETE ${onDelete}`;
      });
    }

    if (fkDupColumn) {
      return {
        error: `The column "${fkDupColumn}" seems to be referencing multiple foreign columns`,
      };
    }

    // add check constraints
    if (checkConstraints.length > 0) {
      checkConstraints.forEach(constraint => {
        if (!constraint.name || !constraint.check) {
          return;
        }

        tableDefSql += `, CONSTRAINT "${constraint.name}" CHECK(${constraint.check})`;
      });
    }

    let sqlCreateTable = `CREATE TABLE "${currentSchema}"."${tableName}" (${tableDefSql});`;

    // add comment to the table using MS_Description property
    if (tableComment && tableComment !== '') {
      const commentSQL = `EXEC sys.sp_addextendedproperty   
      @name = N'MS_Description',   
      @value = N'${tableComment}',   
      @level0type = N'SCHEMA', @level0name = '${currentSchema}',  
      @level1type = N'TABLE',  @level1name = '${tableName}';`;
      sqlCreateTable += `${commentSQL}`;
    }

    return [sqlCreateTable];
  },
  getDropTableSql: (schema: string, table: string) => {
    return `DROP TABLE "${schema}"."${table}"`;
  },
  createSQLRegex,
  getDropSchemaSql: (schema: string) => {
    return `drop schema ${schema};`;
  },
  getCreateSchemaSql: (schema: string) => {
    return `create schema ${schema};`;
  },
  isTimeoutError: () => {
    return false;
  },
  getAlterForeignKeySql: () => {
    return '';
  },
  getCreateFKeySql: () => {
    return '';
  },
  getDropConstraintSql: () => {
    return '';
  },
  getRenameTableSql: () => {
    return '';
  },
  getDropTriggerSql: () => {
    return '';
  },
  getCreateTriggerSql: () => {
    return '';
  },
  getDropSql: () => {
    return '';
  },
  getViewDefinitionSql: () => {
    return '';
  },
  getDropColumnSql: () => {
    return '';
  },
  getAddColumnSql: () => {
    return '';
  },
  getAddUniqueConstraintSql: () => {
    return '';
  },
  getDropNotNullSql: () => {
    return '';
  },
  getSetCommentSql: () => {
    return '';
  },
  getSetColumnDefaultSql: () => {
    return '';
  },
  getSetNotNullSql: () => {
    return '';
  },
  getAlterColumnTypeSql: () => {
    return '';
  },
  getDropColumnDefaultSql: () => {
    return '';
  },
  getRenameColumnQuery: () => {
    return '';
  },
  fetchColumnCastsQuery: '',
  checkSchemaModification: () => {
    return false;
  },
  getCreateCheckConstraintSql: () => {
    return '';
  },
  getCreatePkSql: () => {
    return '';
  },
  getFunctionDefinitionSql: null,
  primaryKeysInfoSql: ({ schemas }) => {
    let whereClause = '';

    if (schemas) {
      whereClause = `WHERE schema_name (schema_id) in (${schemas
        .map(s => `'${s}'`)
        .join(',')})`;
    }
    return `
  SELECT
    schema_name (tab.schema_id) AS table_schema,
    tab.name AS table_name,
    (
      SELECT
        col.name,
        pk.name AS constraint_name
      FROM
        sys.indexes pk
        INNER JOIN sys.index_columns ic ON ic.object_id = pk.object_id
          AND ic.index_id = pk.index_id
        INNER JOIN sys.columns col ON pk.object_id = col.object_id
          AND col.column_id = ic.column_id
      WHERE
        tab.object_id = pk.object_id AND pk.is_primary_key = 1 FOR json path) AS constraints
    FROM
      sys.tables tab
      INNER JOIN sys.indexes pk ON tab.object_id = pk.object_id
        AND pk.is_primary_key = 1
        ${whereClause}
    GROUP BY
      tab.name,
      tab.schema_id,
      tab.object_id
    FOR JSON PATH;
    `;
  },
  checkConstraintsSql: ({ schemas }) => {
    let whereClause = '';

    if (schemas) {
      whereClause = `WHERE schema_name (t.schema_id) in (${schemas
        .map(s => `'${s}'`)
        .join(',')})`;
    }
    return `
SELECT
	con.name AS constraint_name,
	schema_name (t.schema_id) AS table_schema,
	t.name AS table_name,
	col.name AS column_name,
	con.definition AS check_definition
FROM
	sys.check_constraints con
	LEFT OUTER JOIN sys.objects t ON con.parent_object_id = t.object_id
	LEFT OUTER JOIN sys.all_columns col ON con.parent_column_id = col.column_id
		AND con.parent_object_id = col.object_id
${whereClause}
ORDER BY con.name
FOR JSON PATH;
    `;
  },
  uniqueKeysSql: ({ schemas }) => {
    let whereClause = '';

    if (schemas) {
      whereClause = `WHERE schema_name (schema_id) in (${schemas
        .map(s => `'${s}'`)
        .join(',')})`;
    }
    return `
    SELECT
    schema_name (tab.schema_id) AS table_schema,
    tab.name AS table_name,
    (
      SELECT
        col.name,
        idx.name AS constraint_name
      FROM
        sys.indexes idx
        INNER JOIN sys.index_columns ic ON ic.object_id = idx.object_id
          AND ic.index_id = idx.index_id
        INNER JOIN sys.columns col ON idx.object_id = col.object_id
          AND col.column_id = ic.column_id
      WHERE
        tab.object_id = idx.object_id
        AND idx.is_unique_constraint = 1 FOR json path) AS constraints
    FROM
      sys.tables tab
      INNER JOIN sys.indexes idx ON tab.object_id = idx.object_id
        AND idx.is_unique_constraint = 1
      ${whereClause}
    GROUP BY
      tab.name,
      tab.schema_id,
      tab.object_id
    FOR JSON PATH;
    `;
  },
  frequentlyUsedColumns: [],
  getFKRelations: () => {
    return `
SELECT
    fk.name AS constraint_name,
    sch1.name AS [table_schema],
    tab1.name AS [table_name],
    sch2.name AS [ref_table_schema],
    tab2.name AS [ref_table],
    (
        SELECT
            col1.name AS [column],
            col2.name AS [referenced_column]
        FROM sys.foreign_key_columns fkc
        INNER JOIN sys.columns col1
            ON col1.column_id = fkc.parent_column_id AND col1.object_id = tab1.object_id
        INNER JOIN sys.columns col2
            ON col2.column_id = fkc.referenced_column_id AND col2.object_id = tab2.object_id
        WHERE fk.object_id = fkc.constraint_object_id
        FOR JSON PATH
    ) AS column_mapping,
    fk.delete_referential_action_desc AS [on_delete],
    fk.update_referential_action_desc AS [on_update]
FROM sys.foreign_keys fk
INNER JOIN sys.objects obj
    ON obj.object_id = fk.referenced_object_id
INNER JOIN sys.tables tab1
    ON tab1.object_id = fk.parent_object_id
INNER JOIN sys.schemas sch1
    ON tab1.schema_id = sch1.schema_id
INNER JOIN sys.tables tab2
    ON tab2.object_id = fk.referenced_object_id
INNER JOIN sys.schemas sch2
    ON tab2.schema_id = sch2.schema_id for json path;
    `;
  },
  getReferenceOption: () => {
    return '';
  },
  deleteFunctionSql: () => {
    return '';
  },
  getEventInvocationInfoByIDSql: () => {
    return '';
  },
  getDatabaseInfo: '',
  getTableInfo: (tables: QualifiedTable[]) => `
SELECT
	o.name AS table_name,
	s.name AS table_schema,
	table_type = CASE o.type
	WHEN 'U' THEN
		'table'
	WHEN 'V' THEN
		'view'
	ELSE
		'table'
	END
FROM
	sys.objects AS o
	JOIN sys.schemas AS s ON (o.schema_id = s.schema_id)
WHERE
	o.name in (${tables.map(t => `'${t.name}'`).join(',')}) for json path;
  `,
  getDatabaseVersionSql: 'SELECT @@VERSION;',
  permissionColumnDataTypes,
  viewsSupported: false,
  supportedColumnOperators,
  aggregationPermissionsAllowed: false,
  supportedFeatures,
  violationActions,
  defaultRedirectSchema,
};
