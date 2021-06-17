import { AlterFKTableInfo, MySQLTrigger, CreatePKArgs } from './types';

export const getMySQLNameString = (schemaName: string, itemName: string) =>
  `\`${schemaName}\`.\`${itemName}\``;

export const sqlEscapeText = (text?: string | null) => {
  if (!text) {
    return 'NULL';
  }
  return `'%string "${text}" %'`;
};

export const isColTypeString = (colType: string) =>
  [
    'varbinary',
    'binary',
    'varchar',
    'char',
    'enum',
    'set',
    'blob',
    'text',
  ].includes(colType);

export const getAlterForeignKeySql = (
  from: AlterFKTableInfo,
  to: AlterFKTableInfo,
  dropConstraint: string,
  newConstraint: string,
  onUpdate: string,
  onDelete: string
) => `
   alter table ${getMySQLNameString(
     from.schemaName,
     from.tableName
   )} drop foreign key \`${dropConstraint}\`;
   alter table ${getMySQLNameString(to.schemaName, to.tableName)}
   add constraint \`${newConstraint}\` foreign key (${from.columns.join(', ')})
   references ${getMySQLNameString(to.schemaName, to.tableName)}
   (${to.columns.join(', ')}) on update ${onUpdate} on delete ${onDelete};
 `;

export const getCreateFKeySql = (
  from: AlterFKTableInfo,
  to: AlterFKTableInfo,
  newConstraint: string,
  onUpdate: string,
  onDelete: string
) => `
alter table ${getMySQLNameString(to.schemaName, to.tableName)}
add constraint \`${newConstraint}\` foreign key (${from.columns.join(', ')})
references ${getMySQLNameString(to.schemaName, to.tableName)}
(${to.columns.join(', ')}) on update ${onUpdate} on delete ${onDelete};
`;

export const getDropConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} drop constaint ${constraintName};
`;

export const getRenameTableSql = (
  property = 'table',
  oldName: string,
  schemaName: string,
  newName: string
) => `
  alter ${property} ${getMySQLNameString(
  schemaName,
  oldName
)} rename to ${newName};
`;

export const getDropTriggerSql = (tableSchema: string, triggerName: string) => `
DROP TRIGGER IF EXISTS ${getMySQLNameString(tableSchema, triggerName)};
`;

export const getCreateTriggerSql = (
  tableName: string,
  tableSchema: string,
  triggerName: string,
  trigger: MySQLTrigger
) => `
  CREATE TRIGGER \`${triggerName}\`
  ${trigger.action_timing} ${
  trigger.event_manipulation
} ON ${getMySQLNameString(tableSchema, tableName)}
  FOR EACH ${trigger.action_orientation} ${trigger.action_statement};
`;

export const getDropSql = (
  tableName: string,
  schemaName: string,
  property = 'table'
) => `
drop ${property} ${getMySQLNameString(schemaName, tableName)};
`;

export const getDropColumnSql = (
  tableName: string,
  schemaName: string,
  columnName: string
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} drop column \`${columnName}\`;`;

export const getAddColumnSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  columnType: string,
  options?: { nullable: boolean; unique: boolean; default: any }
) => {
  let sql = `alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} add column \`${columnName}\` ${columnType}`;

  if (!options) {
    return sql;
  }

  if (options.nullable) {
    sql += ' null';
  } else {
    sql += ' not null';
  }
  if (options.unique) {
    sql += ' unique';
  }
  if (options.default) {
    let defaultVal = options.default;
    if (isColTypeString(columnType)) {
      defaultVal = `'${options.default}'`;
    }
    sql += defaultVal;
  }

  return sql;
};

export const getAddUniqueConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string,
  columns: string[]
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} add constraint ${constraintName} unique (${columns.join(', ')});
`;

export const getDropNotNullSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  columnType?: string
) => {
  let colType = columnType;
  let sql = `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} modify \`${columnName}\` `;

  if (columnType?.toLowerCase().includes('not null')) {
    const typeSplit = columnType.split('not null');
    colType = typeSplit.map(type => type.trim()).join(' ');
  }
  sql += colType;
  return sql;
};

export const getSetCommentSql = (
  on: 'column' | 'table' | string,
  tableName: string,
  schemaName: string,
  columnName: string,
  comment: string | null,
  columnType?: string
) => {
  const commentStr = sqlEscapeText(comment);

  if (on === 'column') {
    return `alter table ${getMySQLNameString(
      schemaName,
      tableName
    )} modify column \`${columnName}\` ${columnType} comment ${commentStr};`;
  }

  // FIXME: this is only meant to be for on = table
  return `alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} comment = ${commentStr};`;
};

export const getSetColumnDefaultSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  defaultValue: any,
  columnType: string
) => {
  let defVal = defaultValue;
  if (isColTypeString(columnType)) {
    defVal = `'${defaultValue}'`;
  }
  return `
    alter table ${getMySQLNameString(
      schemaName,
      tableName
    )} alter \`${columnName}\` set default ${defVal};
  `;
};

export const getSetNotNullSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  columnType?: string
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} modify \`${columnName}\` ${columnType} not null;
`;

export const getAlterColumnTypeSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  columnType: string
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} modify column \`${columnName}\` ${columnType};  
`;

export const getDropColumnDefaultSql = (
  tableName: string,
  schemaName: string,
  columnName: string
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} alter \`${columnName}\` drop default;
`;

export const getRenameColumnQuery = (
  tableName: string,
  schemaName: string,
  newName: string,
  oldName: string,
  columnType?: string
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} change \`${oldName}\` \`${newName}\` ${columnType};
`;

export const checkSchemaModification = (sql: string) => {
  const sqlStatements = sql
    .toLowerCase()
    .split(';')
    .map(sqlStr => sqlStr.trim());

  return sqlStatements.some(
    statement =>
      statement.startsWith('create ') ||
      statement.startsWith('alter ') ||
      statement.startsWith('drop ')
  );
};

export const getCreateCheckConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string,
  check: string
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} add constraint \`${constraintName}\` check ${check};
`;

export const getCreatePkSql = ({
  schemaName,
  tableName,
  selectedPkColumns,
}: CreatePKArgs) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} add primary key (${selectedPkColumns.join(', ')});
`;

export const getAlterPkSql = () => {
  return '';
};

export const getCreateTableQueries = (
  currentSchema: string,
  tableName: string,
  columns: any[],
  primaryKeys: (number | string)[],
  foreignKeys: any[],
  uniqueKeys: any[],
  checkConstraints: any[],
  tableComment?: string
) => {
  const currentCols = columns.filter(c => c.name !== '');

  const pKeys = primaryKeys
    .filter(p => p !== '')
    .map(p => currentCols[p as number].name);

  let tableDefSql = '';
  for (let i = 0; i < currentCols.length; i++) {
    tableDefSql += `\`${currentCols[i].name}\` ${currentCols[i].type}`;

    // check if column is nullable
    if (!currentCols[i].nullable) {
      tableDefSql += ' NOT NULL';
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
    tableDefSql += pKeys.map(col => `\`${col}\``).join(',');
    tableDefSql += ') ';
  }

  // add foreign keys
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
          lCols.push(`\`${columns[cm.column as number].name}\``);
          rCols.push(`\`${cm.refColumn}\``);
        });

      if (lCols.length === 0) {
        return;
      }

      tableDefSql += `, FOREIGN KEY (${lCols.join(', ')}) REFERENCES \`${
        fk.refSchemaName
      }\`.\`${refTableName}\`(${rCols.join(
        ', '
      )}) ON UPDATE ${onUpdate} ON DELETE ${onDelete}`;
    });
  }

  if (fkDupColumn) {
    return {
      error: `The column "${fkDupColumn}" seems to be referencing multiple foreign columns`,
    };
  }

  // add unique keys
  const numUniqueConstraints = uniqueKeys.length;
  if (numUniqueConstraints > 0) {
    uniqueKeys.forEach(uk => {
      if (!uk.length) {
        return;
      }

      const uniqueColumns = uk.map((c: number) => `\`${columns[c].name}\``);
      tableDefSql += `, UNIQUE (${uniqueColumns.join(', ')})`;
    });
  }

  // add check constraints
  if (checkConstraints.length > 0) {
    checkConstraints.forEach(constraint => {
      if (!constraint.name || !constraint.check) {
        return;
      }

      tableDefSql += `, CONSTRAINT \`${constraint.name}\` CHECK (${constraint.check})`;
    });
  }

  let sqlCreateTable = `CREATE TABLE ${getMySQLNameString(
    currentSchema,
    tableName
  )} (${tableDefSql})`;

  // add comment
  if (tableComment && tableComment !== '') {
    sqlCreateTable += `, COMMENT = ${sqlEscapeText(tableComment)};`;
  } else {
    sqlCreateTable += ';';
  }

  return [sqlCreateTable];
};

export const getDropSchemaSql = (schemaName: string) => `
  drop schema \`${schemaName}\` cascade;
`;

export const getCreateSchemaSql = (schemaName: string) => `
  create schema if not exists \`${schemaName}\`;
`;

export const getDropTableSql = (schemaName: string, tableName: string) => `
  drop table ${getMySQLNameString(schemaName, tableName)};
`;

const whereQuery = (
  options: { schemas: string[] },
  schemaColName: string,
  start = 'WHERE'
) =>
  options?.schemas.length
    ? `
${start} ${schemaColName} IN (${options.schemas
        .map(schema => `'${schema}'`)
        .join(', ')})
`
    : '';

export const getFetchTablesListQuery = (options: { schemas: string[] }) => {
  return `
SELECT
	COALESCE(JSON_ARRAYAGG(info.tables), '[]')
FROM (
	SELECT
		json_object('table_schema', t.table_schema, 'table_name', t.table_name, 'table_type', t.table_type, 'comment', t.table_comment, 'columns', columns_info.columns, 'view_info', views_info.view) AS tables
	FROM
		TABLES t
		JOIN (
			SELECT
				table_name,
				table_schema,
				JSON_ARRAYAGG(json_object('col_name', COLUMN_NAME, 'table_name', TABLE_NAME, 'table_schema', TABLE_SCHEMA, 'ordinal_position', ordinal_position, 'column_default', column_default, 'is_nullable', is_nullable, 'data_type', column_type, 'data_type_name', column_type, 'column_comment', column_comment, 'COLUMN_KEY', COLUMN_KEY, 'extra', extra)) AS columns
			FROM
				COLUMNS
			GROUP BY
				table_name,
				table_schema) columns_info ON (t.TABLE_SCHEMA = columns_info.table_schema
				AND t. \`TABLE_NAME\` = columns_info.table_name)
			JOIN (
				SELECT
					TABLE_SCHEMA,
					table_name,
					json_object('table_name', table_name, 'view_definition', view_definition, 'is_updatable', is_updatable, 'is_insertable_into', is_updatable) AS VIEW
				FROM
					\`VIEWS\`) AS views_info ON t.table_schema = views_info.TABLE_SCHEMA
					AND t. \`TABLE_NAME\` = views_info. \`TABLE_NAME\`
			${whereQuery(options, 't.table_schema')}
) AS info;
  `;
};

export const getFKRelations = (options: { schemas: string[] }) => {
  return `
SELECT
	rc.CONSTRAINT_SCHEMA AS table_schema,
	rc.table_name AS table_name,
	rc.constraint_name AS constraint_name,
	rc.REFERENCED_TABLE_NAME AS ref_table,
	rc.UNIQUE_CONSTRAINT_SCHEMA AS ref_table_table_schema,
	rc.UPDATE_RULE AS on_update,
	rc.DELETE_RULE AS
	on_delete,
	JSON_OBJECTAGG(kcu. \`COLUMN_NAME\`, kcu.REFERENCED_COLUMN_NAME) AS column_mapping
FROM
	information_schema.REFERENTIAL_CONSTRAINTS rc
  JOIN information_schema.KEY_COLUMN_USAGE kcu ON (rc. \`CONSTRAINT_NAME\` = kcu. \`CONSTRAINT_NAME\`)
  ${whereQuery(options, 'rc.CONSTRAINT_SCHEMA')}
GROUP BY
	constraint_name;
  `;
};

// yes, it's not the best way to do this. we could as well fetch information about
// both unique and primary keys in one sql. It was done for the sake reusing existing code
// as much as possible
const getKeysSql = (
  type: 'PRIMARY KEY' | 'UNIQUE',
  options: { schemas: string[] }
) => `
SELECT
	COALESCE(JSON_ARRAYAGG(info), '[]')
FROM (
	SELECT
		JSON_OBJECT('table_schema', stat.table_schema, 'table_name', stat.table_name, 'constraint_name', stat.index_name, 'columns', concat("[", group_concat(stat.column_name ORDER BY stat.seq_in_index separator ', '),
"]")) AS info
FROM
	information_schema.statistics stat
	JOIN information_schema.table_constraints tco ON stat.table_schema = tco.table_schema
		AND stat.table_name = tco.table_name
		AND stat.index_name = tco.constraint_name
WHERE
	stat.non_unique = 0
	AND constraint_type = "${type}"
	${whereQuery(options, 'stat.table_schema', 'AND')}
GROUP BY
	stat.table_schema,
	stat.table_name,
	stat.index_name,
	tco.constraint_type
ORDER BY
	stat.table_schema,
	stat.table_name) AS info;
`;

export const primaryKeysInfoSql = (options: { schemas: string[] }) =>
  getKeysSql('PRIMARY KEY', options);

export const uniqueKeysSql = (options: { schemas: string[] }) =>
  getKeysSql('UNIQUE', options);

export const schemaListSql = (
  schemas: string[]
) => `SELECT schema_name FROM information_schema.schemata WHERE
schema_name NOT IN ('information_schema','mysql','sys','performance_schema') ${
  schemas.length ? `AND schema_name IN (${schemas.join(',')})` : ''
} ORDER BY schema_name ASC;`;

export const getAdditionalColumnsInfoQuerySql = (schemaName: string) =>
  `SELECT column_name, table_name FROM information_schema.columns where table_schema = '${schemaName}';`;
