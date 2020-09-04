import { AlterFKTableInfo, MySQLTrigger, CreatePKArgs } from './types';
import { isSQLFunction } from '../postgresql/sqlUtils';

export const getMySQLNameString = (schemaName: string, itemName: string) =>
  `\`${schemaName}\`.\`${itemName}\``;

export const escapeText = (text?: string | null) => {
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
    if (isColTypeString(columnType) && !isSQLFunction(options.default)) {
      // todo : check for mysql
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
) => `
  alter table ${getMySQLNameString(
    schemaName,
    tableName
  )} modify \`${columnName}\` ${columnType};
`;

export const getSetCommentSql = (
  on: 'column' | 'table' | string,
  tableName: string,
  schemaName: string,
  columnName: string,
  comment: string | null,
  columnType?: string
) => {
  const commentStr = escapeText(comment);

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
  if (isColTypeString(columnType) && !isSQLFunction(defaultValue)) {
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
