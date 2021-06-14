import { currentDriver } from '../../../../dataSources';
import { services } from '../../../../dataSources/services';

const getSQLValue = value => {
  const quotedStringRegex = /^".*"$/;

  let sqlValue = value;
  if (!quotedStringRegex.test(value)) {
    sqlValue = value.toLowerCase();
  }

  return sqlValue.replace(/['"]+/g, '');
};

export const removeCommentsSQL = sql => {
  const commentsSQLRegex = /(--[^\r\n]*)|(\/\*[\w\W]*?(?=\*\/)\*\/)/; // eslint-disable-line
  const regExp = commentsSQLRegex;
  const comments = sql.match(new RegExp(regExp, 'gmi'));

  if (!comments || !comments.length) return sql;

  return comments.reduce((acc, comment) => acc.replace(comment, ''), sql);
};

const getDefaultSchema = driver => {
  if (driver === 'postgres') return 'public';
  if (driver === 'mssql') return 'dbo';
};

/**
 * parses create table|function|view sql
 * @param {string} sql
 * @param {typeof currentDriver} [driver=currentDriver]
 * @return {Array<{type: "table"|"function"|"view", schema: string, table: string, isPartition: boolean}>}
 */
export const parseCreateSQL = (sql, driver = currentDriver) => {
  const _objects = [];
  const regExp = services[driver].createSQLRegex;
  for (const result of sql.matchAll(regExp)) {
    const { type, schema, table, tableWithSchema, partition } =
      result.groups ?? {};
    if (!type || !(table || tableWithSchema)) continue;
    _objects.push({
      type: type.toLowerCase(),
      schema: getSQLValue(schema || getDefaultSchema(driver)),
      name: getSQLValue(table || tableWithSchema),
      isPartition: !!partition,
    });
  }
  return _objects;
};
