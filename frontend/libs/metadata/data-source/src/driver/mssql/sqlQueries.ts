import {
  getSchemasWhereClause,
  getTablesWhereClause,
} from '../common/sqlQueries';
import { TableORSchemaArg } from '../types';

export const getSqlKey = (
  opts: TableORSchemaArg,
  check: 'is_unique_constraint' | 'is_primary_key',
) => {
  return `
  -- test_id = ${'schemas' in opts ? 'multi' : 'single'}_${
    check !== 'is_primary_key' ? 'unique' : 'primary'
  }_key
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
        AND idx.${check} = 1 FOR json path) AS constraints
    FROM
      sys.tables tab
      INNER JOIN sys.indexes idx ON tab.object_id = idx.object_id
        AND idx.${check} = 1
        ${
          'schemas' in opts
            ? getSchemasWhereClause(opts.schemas)('schema_name (schema_id)')
            : getTablesWhereClause(opts.tables)({
                name: 'tab.name',
                schema: 'schema_name (schema_id)',
              })
        }
    GROUP BY
      tab.name,
      tab.schema_id,
      tab.object_id
    FOR JSON PATH;
    `;
};

export const msSqlQueries = {
  // uniqueKeysSql(options: TableORSchemaArg): string {
  //   return getSqlKey(options, 'is_unique_constraint');
  // },
  checkConstraintsSql(options: TableORSchemaArg): string {
    return `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_check_constraint
    SELECT
      con.name AS constraint_name,
      schema_name (t.schema_id) AS table_schema,
      t.name AS table_name,
      col.name AS column_name,
      con.definition AS 'check'
    FROM
      sys.check_constraints con
      LEFT OUTER JOIN sys.objects t ON con.parent_object_id = t.object_id
      LEFT OUTER JOIN sys.all_columns col ON con.parent_column_id = col.column_id
        AND con.parent_object_id = col.object_id
    ${
      'schemas' in options
        ? getSchemasWhereClause(options.schemas)('schema_name (t.schema_id)')
        : getTablesWhereClause(options.tables)({
            name: 't.name',
            schema: 'schema_name (t.schema_id)',
          })
    }
    ORDER BY con.name
    FOR JSON PATH, INCLUDE_NULL_VALUES;
    `;
  },
};
