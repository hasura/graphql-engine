import React from 'react';
import { DeepRequired } from 'ts-essentials';
import { DataSourcesAPI } from '../..';
import { QualifiedTable } from '../../../metadata/types';
import {
  TableColumn,
  Table,
  BaseTableColumn,
  SupportedFeaturesType,
  FrequentlyUsedColumn,
  ViolationActions,
} from '../../types';
import {
  generateTableRowRequest,
  operators,
  generateRowsCountRequest,
} from './utils';

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
const createSQLRegex = /create\s*(?:|or\s*replace)\s*(?<type>view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((?<schema>\"?\w+\"?)\.(?<nameWithSchema>\"?\w+\"?)|(?<name>\"?\w+\"?))\s*(?<partition>partition\s*of)?/gim;

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

export const supportedFeatures: DeepRequired<SupportedFeaturesType> = {
  driver: {
    name: 'mssql',
    fetchVersion: {
      enabled: false,
    },
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
      columnTypeSelector: false,
    },
    browse: {
      enabled: true,
      customPagination: true,
      aggregation: true,
    },
    insert: {
      enabled: false,
    },
    modify: {
      editableTableName: false,
      enabled: true,
      columns: {
        view: true,
        edit: true,
        graphqlFieldName: false,
        frequentlyUsedColumns: false,
      },
      readOnly: false,
      computedFields: false,
      primaryKeys: {
        view: true,
        edit: true,
      },
      foreignKeys: {
        view: true,
        edit: true,
      },
      uniqueKeys: {
        view: true,
        edit: true,
      },
      triggers: false,
      checkConstraints: {
        view: true,
        edit: false,
      },
      indexes: {
        view: false,
        edit: false,
      },
      customGqlRoot: false,
      setAsEnum: false,
      untrack: true,
      delete: true,
      comments: {
        view: true,
        edit: false,
      },
    },
    relationships: {
      enabled: true,
      track: true,
      remoteRelationships: false,
    },
    permissions: {
      enabled: true,
      aggregation: false,
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
    // ## Note on fetching MSSQL table comments
    // LEFT JOIN (select * from sys.extended_properties where "minor_id"= 0) AS e ON e.major_id = obj.object_id
    // this would filter out column comments, but if there are multiple extended properties with same major id and minor_id=0;
    // ie, multiple properties on the table name (as shown below) can cause confusion and then it would pick only the first result.
    // * case 1
    // EXEC sys.sp_addextendedproperty
    // @name=N'TableDescription',
    // @value=N'Album Table is used for listing albums' ,
    // @level0type=N'SCHEMA', @level0name=N'dbo',
    // @level1type=N'TABLE', @level1name=N'Album';
    // * case 2
    // EXEC sys.sp_addextendedproperty
    // @name=N'TableDescription2', -- ==> this is possible with mssql
    // @value=N'some other property description' ,
    // @level0type=N'SCHEMA', @level0name=N'dbo',
    // @level1type=N'TABLE', @level1name=N'Album';

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
		LEFT JOIN (select * from sys.extended_properties where minor_id = 0) AS e ON e.major_id = obj.object_id
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
            t.name AS data_type_name,
            sch.name AS table_schema,
            obj.name AS table_name,
            ep.value AS comment,
            ep.name AS extended_property_name_comment
        FROM
            sys.columns a
            LEFT JOIN sys.default_constraints ad ON (a.column_id = ad.parent_column_id AND a.object_id = ad.parent_object_id)
            JOIN sys.types t ON a.user_type_id = t.user_type_id
            LEFT JOIN sys.extended_properties ep ON (ep.major_id = a.object_id AND ep.minor_id = a.column_id)
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
  getAlterForeignKeySql: (
    from: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    to: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    dropConstraint: string,
    newConstraint: string,
    onUpdate: string,
    onDelete: string
  ) => {
    return `
    BEGIN transaction;
    ALTER TABLE "${from.schemaName}"."${from.tableName}"
    DROP CONSTRAINT IF EXISTS "${dropConstraint}";
    ALTER TABLE "${from.schemaName}"."${from.tableName}"
    ADD CONSTRAINT "${newConstraint}"
    FOREIGN KEY (${from.columns.join(', ')})
    REFERENCES "${to.schemaName}"."${to.tableName}" (${to.columns.join(', ')}) 
    ON UPDATE ${onUpdate} ON DELETE ${onDelete};
    COMMIT transaction;
    `;
  },
  getCreateFKeySql: (
    from: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    to: {
      tableName: string;
      schemaName: string;
      columns: string[];
    },
    newConstraint: string,
    onUpdate: string,
    onDelete: string
  ) => {
    return `
    ALTER TABLE "${from.schemaName}"."${from.tableName}"
    ADD CONSTRAINT "${newConstraint}"
    FOREIGN KEY (${from.columns.join(', ')})
    REFERENCES "${to.schemaName}"."${to.tableName}" (${to.columns.join(', ')})
    ON UPDATE ${onUpdate} ON DELETE ${onDelete}`;
  },
  getDropConstraintSql: (
    tableName: string,
    schemaName: string,
    constraintName: string
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}" DROP CONSTRAINT "${constraintName}"`;
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
  getDropSql: (tableName: string, schemaName: string, property = 'table') => {
    return `DROP ${property} "${schemaName}"."${tableName}";`;
  },
  getViewDefinitionSql: () => {
    return '';
  },
  getDropColumnSql: (
    tableName: string,
    schemaName: string,
    columnName: string
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}"
    DROP COLUMN "${columnName}"`;
  },
  getAddColumnSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType: string,
    options?: {
      nullable: boolean;
      unique: boolean;
      default: any;
      sqlGenerator?: FrequentlyUsedColumn['dependentSQLGenerator'];
    },
    constraintName?: string
  ) => {
    let sql = `ALTER TABLE "${schemaName}"."${tableName}" ADD "${columnName}" ${columnType}`;
    if (!options) {
      return sql;
    }
    if (options.nullable) {
      sql += ` NULL`;
    } else {
      sql += ` NOT NULL`;
    }
    if (options.unique) {
      sql += ` UNIQUE`;
    }
    if (options.default) {
      sql += ` CONSTRAINT "${constraintName}" DEFAULT '${options.default}' WITH VALUES`;
    }
    return sql;
  },
  getAddUniqueConstraintSql: (
    tableName: string,
    schemaName: string,
    constraintName: string,
    columns: string[]
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}"
    ADD CONSTRAINT "${constraintName}"
    UNIQUE (${columns.join(',')})`;
  },
  getDropNotNullSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType?: string
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}" ALTER COLUMN "${columnName}"  ${columnType} NULL`;
  },
  getSetCommentSql: (
    on: 'column' | 'table' | string,
    tableName: string,
    schemaName: string,
    comment: string | null,
    columnName?: string
  ) => {
    const dropCommonCommentStatement = `IF EXISTS (SELECT NULL FROM SYS.EXTENDED_PROPERTIES WHERE [major_id] = OBJECT_ID('${tableName}') AND [name] = N'${on}_comment_${schemaName}_${tableName}_${columnName}' AND [minor_id] = (SELECT [column_id] FROM SYS.COLUMNS WHERE [name] = '${columnName}' AND [object_id] = OBJECT_ID('${tableName}')))    
        EXECUTE sp_dropextendedproperty   
        @name = N'${on}_comment_${schemaName}_${tableName}_${columnName}',   
        @level0type = N'SCHEMA', @level0name = '${schemaName}'
    `;
    const commonCommentStatement = `
        exec sys.sp_addextendedproperty   
        @name = N'${on}_comment_${schemaName}_${tableName}_${columnName}',   
        @value = N'${comment}',   
        @level0type = N'SCHEMA', @level0name = '${schemaName}'
    `;
    if (on === 'column') {
      return `${dropCommonCommentStatement},@level1type = N'TABLE',  @level1name = '${tableName}',@level2type = N'COLUMN', @level2name = '${columnName}';
      ${commonCommentStatement},@level1type = N'TABLE',  @level1name = '${tableName}',@level2type = N'COLUMN', @level2name = '${columnName}'`;
    }
    // FIXME: Comment on mssql table and function is not implemented yet.
    return '';
  },
  getSetColumnDefaultSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    defaultValue: any,
    constraintName: string
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}" DROP CONSTRAINT IF EXISTS "${constraintName}";
    ALTER TABLE "${schemaName}"."${tableName}" ADD CONSTRAINT "${constraintName}" DEFAULT ${defaultValue} FOR "${columnName}"`;
  },
  getSetNotNullSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType: string
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}" ALTER COLUMN "${columnName}"  ${columnType} NOT NULL`;
  },
  getAlterColumnTypeSql: (
    tableName: string,
    schemaName: string,
    columnName: string,
    columnType: string,
    wasNullable?: boolean
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}" ALTER COLUMN "${columnName}" ${columnType} ${
      !wasNullable ? `NOT NULL` : ``
    }`;
  },
  getDropColumnDefaultSql: (
    tableName: string,
    schemaName: string,
    columnName?: string,
    constraintName?: string
  ) => {
    return `ALTER TABLE "${schemaName}"."${tableName}" DROP CONSTRAINT "${constraintName}"`;
  },
  getRenameColumnQuery: (
    tableName: string,
    schemaName: string,
    newName: string,
    oldName: string
  ) => {
    return `sp_rename '[${schemaName}].[${tableName}].[${oldName}]', '${newName}', 'COLUMN'`;
  },
  fetchColumnCastsQuery: '',
  checkSchemaModification: () => {
    return false;
  },
  getCreateCheckConstraintSql: () => {
    return '';
  },
  getCreatePkSql: ({
    schemaName,
    tableName,
    selectedPkColumns,
    constraintName,
  }: {
    schemaName: string;
    tableName: string;
    selectedPkColumns: string[];
    constraintName?: string;
  }) => {
    return `ALTER TABLE "${schemaName}"."${tableName}"
    ADD CONSTRAINT "${constraintName}"
    PRIMARY KEY (${selectedPkColumns.map(pkc => `"${pkc}"`).join(',')})`;
  },
  getAlterPkSql: ({
    schemaName,
    tableName,
    selectedPkColumns,
    constraintName,
  }: {
    schemaName: string;
    tableName: string;
    selectedPkColumns: string[];
    constraintName: string; // compulsory for PG
  }) => {
    return `BEGIN TRANSACTION;
    ALTER TABLE "${schemaName}"."${tableName}" DROP CONSTRAINT "${constraintName}";
    ALTER TABLE "${schemaName}"."${tableName}"
      ADD CONSTRAINT "${constraintName}" PRIMARY KEY (${selectedPkColumns
      .map(pkc => `"${pkc}"`)
      .join(', ')});
    
    COMMIT TRANSACTION;`;
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
  getReferenceOption: (opt: string) => {
    return opt;
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
  supportedFeatures,
  violationActions,
  defaultRedirectSchema,
  generateRowsCountRequest,
};
