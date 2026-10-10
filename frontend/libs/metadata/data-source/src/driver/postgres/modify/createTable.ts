import { PostgresFamilyDriver } from '@hasura/shared/types';
import { consoleDataTypeToSQLTypeMap, PostgresTable } from '../types';
import {
  DependentSQLGeneratorResult,
  ModifyTableColumnArgs,
  ModifyTableProps,
  ModifyTableArgs,
} from '../../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getTableDisplayName } from '@hasura/shared/utils';

// No `g` flag: a global regex keeps `lastIndex` between `.test()` calls, which
// made every other function default (e.g. a second `now()`) get quoted.
const SQL_FUNCTION_REGEX = /.*\(\)$/m;

const isSQLFunction = (str: unknown | undefined) =>
  Boolean(str) && typeof str === 'string' && SQL_FUNCTION_REGEX.test(str);

// Already a SQL literal or expression: `'text'`, `'a'::text`, `0::bigint`.
const isSQLExpression = (str: string) =>
  (str.startsWith("'") && str.endsWith("'")) || str.includes('::');

/** ` DEFAULT <value>`: numeric/boolean types, function calls and SQL
 *  expressions are used as-is; anything else is quoted as a string literal. */
export const buildDefaultStatement = (column: ModifyTableColumnArgs) => {
  if (!column.default?.value) {
    return '';
  }

  const value = String(column.default.value);
  if (
    consoleDataTypeToSQLTypeMap.boolean.includes(column.type) ||
    consoleDataTypeToSQLTypeMap.float.includes(column.type) ||
    consoleDataTypeToSQLTypeMap.number.includes(column.type) ||
    consoleDataTypeToSQLTypeMap.integer.includes(column.type) ||
    isSQLFunction(value) ||
    isSQLExpression(value)
  ) {
    return ` DEFAULT ${value}`;
  }

  return ` DEFAULT '${value.replace(/'/g, "''")}'`;
};

const buildCreateTableQueries = ({
  table,
  columns,
  primaryKeys,
  foreignKeys,
  uniqueKeys,
  checkConstraints,
}: ModifyTableArgs) => {
  const currentCols = columns.filter((c) => c.name !== '');
  let hasUUIDDefault = false;

  const pKeys = primaryKeys.map((p) => currentCols[p as number].name);

  const columnSpecificSql: DependentSQLGeneratorResult[] = [];

  let tableDefSql = '';
  for (let i = 0; i < currentCols.length; i++) {
    tableDefSql += `"${currentCols[i].name}" ${currentCols[i].type}`;

    // check if column is nullable
    if (!currentCols[i].nullable) {
      tableDefSql += ' NOT NULL';
    }

    // check if column has a default value
    if (
      currentCols[i].default !== undefined &&
      currentCols[i].default?.value !== ''
    ) {
      tableDefSql += buildDefaultStatement(currentCols[i]);
      if (currentCols[i].type === 'uuid') {
        hasUUIDDefault = true;
      }
    }

    // check if column has dependent sql
    const depGen = currentCols[i].dependentSQLGenerator;
    if (depGen) {
      const dependentSql = depGen(table, currentCols[i].name);
      columnSpecificSql.push(dependentSql);
    }

    tableDefSql += i === currentCols.length - 1 ? '' : ', ';
  }

  // add primary key
  if (pKeys.length > 0) {
    tableDefSql += ', PRIMARY KEY (';
    tableDefSql += pKeys.map((col) => `"${col}"`).join(',');
    tableDefSql += ') ';
  }

  // add foreign keys
  foreignKeys
    .filter(
      (fk) =>
        fk.referenceTable &&
        fk.columnMappings.filter((cm) => cm.from && cm.to).length > 0,
    )
    .forEach((fk, _i) => {
      const rCols: string[] = [];
      const lCols: string[] = [];
      const onUpdate = fk.onUpdate || 'restrict';
      const onDelete = fk.onDelete || 'restrict';

      fk.columnMappings
        .filter((columnMap) => columnMap.from && columnMap.to)
        .forEach((cm) => {
          lCols.push(`"${cm.from}"`);
          rCols.push(`"${cm.to}"`);
        });

      if (lCols.length === 0) {
        return;
      }

      const refTable = getTableDisplayName(fk.referenceTable, '"', '.');
      tableDefSql += `, FOREIGN KEY (${lCols.join(', ')}) REFERENCES ${refTable}(${rCols.join(
        ', ',
      )}) ON UPDATE ${onUpdate} ON DELETE ${onDelete}`;
    });

  // add unique keys
  const numUniqueConstraints = uniqueKeys.length;
  if (numUniqueConstraints > 0) {
    uniqueKeys.forEach((uk) => {
      if (!uk.length) {
        return;
      }

      const uniqueColumns = uk.map((c: number) => `"${columns[c].name}"`);
      tableDefSql += `, UNIQUE (${uniqueColumns.join(', ')})`;
    });
  }

  // add check constraints
  if (checkConstraints.length > 0) {
    checkConstraints.forEach((constraint) => {
      if (!constraint.name || !constraint.check) {
        return;
      }

      tableDefSql += `, CONSTRAINT "${constraint.name}" CHECK (${constraint.check})`;
    });
  }

  const tableName = getTableDisplayName(table, '"', '.');
  const upSQLs: string[] = [`CREATE TABLE ${tableName} (${tableDefSql});`];
  const downSQLs: string[] = [`DROP TABLE ${tableName};`];

  columnSpecificSql.forEach((csql) => {
    upSQLs.push(csql.upSql);
    if (csql.downSql) {
      downSQLs.unshift(csql.downSql);
    }
  });

  if (hasUUIDDefault) {
    const sqlCreateExtension = 'CREATE EXTENSION IF NOT EXISTS pgcrypto;';

    upSQLs.push(sqlCreateExtension);
  }

  return {
    up: [
      {
        sql: upSQLs.join('\n'),
      },
    ],
    down: [
      {
        sql: downSQLs.join('\n'),
      },
    ],
  };
};

export const createTableCurry = (kind: PostgresFamilyDriver) => {
  return async ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    args,
  }: ModifyTableProps) => {
    const source = { name: dataSourceName, kind };
    const queries = buildCreateTableQueries(args);
    const pgTable = args.table as PostgresTable;

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `create_table_${pgTable.schema}_${pgTable.name}`,
        ...queries,
      },
    }).then(() => true);
  };
};
