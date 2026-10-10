import { isPostgresFlavour } from '@hasura/metadata/helpers';
import { SupportedDriver } from '@hasura/shared/types';
import { ParseCreateSchemaSQLResult } from '../types';

const commentsSQLRegex = /(--[^\r\n]*)|(\/\*[\w\W]*?(?=\*\/)\*\/)/gim;

const getSQLValue = (value: string): string => {
  const quotedStringRegex = /^".*"$/;

  let sqlValue = value;
  if (!quotedStringRegex.test(value)) {
    sqlValue = value?.toLowerCase() ?? '';
  }

  return sqlValue.replace(/['"]+/g, '');
};

export const removeCommentsSQL = (sql: string) => {
  const comments = sql.match(commentsSQLRegex);

  if (!comments || !comments.length) return sql;

  return comments.reduce((acc, comment) => acc.replace(comment, ''), sql);
};

const getDefaultSchema = (driver: SupportedDriver) => {
  if (isPostgresFlavour(driver)) return 'public';
  if (driver === 'mssql') return 'dbo';

  return 'public';
};

/**
 * parses create table|function|view sql
 */
export const parseCreateSQL = (
  sql: string,
  driver: SupportedDriver,
  createSQLRegex: RegExp,
): ParseCreateSchemaSQLResult[] => {
  const _objects: ParseCreateSchemaSQLResult[] = [];
  const sanitizedSql = removeCommentsSQL(sql);
  for (const result of sanitizedSql.matchAll(createSQLRegex)) {
    const { type, schema, name, nameWithSchema, partition } =
      result.groups ?? {};
    if (!type || !(name || nameWithSchema)) continue;

    _objects.push({
      type: type.toLowerCase() as 'table' | 'view' | 'function',
      schema: getSQLValue(schema || getDefaultSchema(driver)),
      name: getSQLValue(name || nameWithSchema),
      isPartition: !!partition,
    });
  }

  return _objects;
};
