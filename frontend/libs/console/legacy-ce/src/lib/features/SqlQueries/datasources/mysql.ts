import type { TableORSchemaArg } from '../../../dataSources/types';
import { QualifiedTable } from '../../../metadata/types';
import type { DatasourceSqlQueries } from '.';
import { getSchemasWhereClause, getTablesWhereClause } from './common';

const getKeysSql = (
  type: 'PRIMARY KEY' | 'UNIQUE',
  options: TableORSchemaArg
) => `
-- test_id = ${'schemas' in options ? 'multi' : 'single'}_${
  type === 'UNIQUE' ? 'unique' : 'primary'
}_key
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
    ${
      'schemas' in options
        ? getSchemasWhereClause(options.schemas)('stat.table_schema')
        : getTablesWhereClause(options.tables)({
            name: 'stat.table_name',
            schema: 'stat.table_schema',
          })
    }
  GROUP BY
      stat.table_schema,
      stat.table_name,
      stat.index_name,
      tco.constraint_type
  ORDER BY
      stat.table_schema,
      stat.table_name) AS info;
  `;

export const mySqlQueries: DatasourceSqlQueries = {
  getFetchTablesListQuery(options: TableORSchemaArg): string {
    return `
-- test_id = ${'schemas' in options ? 'multi' : 'single'}_table
SELECT
	COALESCE(JSON_ARRAYAGG(info.tables), '[]')
FROM (
	SELECT
		json_object('table_schema', t.table_schema, 'table_name', t.table_name, 'table_type', t.table_type, 'comment', t.table_comment, 'columns', columns_info.columns, 'view_info', views_info.view) AS tables
	FROM
		information_schema.TABLES t
		JOIN (
			SELECT
				table_name,
				table_schema,
				JSON_ARRAYAGG(json_object('col_name', COLUMN_NAME, 'table_name', TABLE_NAME, 'table_schema', TABLE_SCHEMA, 'ordinal_position', ordinal_position, 'column_default', column_default, 'is_nullable', is_nullable, 'data_type', column_type, 'data_type_name', column_type, 'column_comment', column_comment, 'COLUMN_KEY', COLUMN_KEY, 'extra', extra)) AS columns
			FROM
				information_schema.COLUMNS
			GROUP BY
				table_name,
				table_schema) columns_info ON (t.TABLE_SCHEMA = columns_info.table_schema
				AND t.TABLE_NAME = columns_info.table_name)
			JOIN (
				SELECT
					TABLE_SCHEMA,
					table_name,
					json_object('table_name', table_name, 'view_definition', view_definition, 'is_updatable', is_updatable, 'is_insertable_into', is_updatable) AS VIEW
				FROM
					information_schema.VIEWS) AS views_info ON t.table_schema = views_info.TABLE_SCHEMA
					AND t.TABLE_NAME = views_info.TABLE_NAME
      ${
        'schemas' in options
          ? getSchemasWhereClause(options.schemas)('t.table_schema')
          : getTablesWhereClause(options.tables)({
              name: 't.table_name',
              schema: 't.table_schema',
            })
      }
) AS info;
  `;
  },
  primaryKeysInfoSql(options: TableORSchemaArg): string {
    return getKeysSql('PRIMARY KEY', options);
  },
  uniqueKeysSql(options: TableORSchemaArg): string {
    return getKeysSql('UNIQUE', options);
  },
  checkConstraintsSql(options): string {
    return `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_check_constraint
    select "[]"`;
  },
  getFKRelations(options: TableORSchemaArg): string {
    return `
  -- test_id = ${'schemas' in options ? 'multi' : 'single'}_foreign_key
  SELECT
  rc.CONSTRAINT_SCHEMA AS table_schema,
  rc.table_name AS table_name,
  rc.constraint_name AS constraint_name,
  rc.REFERENCED_TABLE_NAME AS ref_table,
  rc.UNIQUE_CONSTRAINT_SCHEMA AS ref_table_table_schema,
  rc.UPDATE_RULE AS on_update,
  rc.DELETE_RULE AS on_delete,
  JSON_OBJECTAGG(kcu.COLUMN_NAME, kcu.REFERENCED_COLUMN_NAME) AS column_mapping
FROM
  information_schema.REFERENTIAL_CONSTRAINTS rc
  JOIN information_schema.KEY_COLUMN_USAGE kcu ON (rc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME)
${
  'schemas' in options
    ? getSchemasWhereClause(options.schemas)([
        'rc.CONSTRAINT_SCHEMA',
        'rc.UNIQUE_CONSTRAINT_SCHEMA',
      ])
    : getTablesWhereClause(options.tables)([
        { name: 'rc.table_name', schema: 'rc.CONSTRAINT_SCHEMA' },
        {
          name: 'rc.REFERENCED_TABLE_NAME',
          schema: 'rc.UNIQUE_CONSTRAINT_SCHEMA',
        },
      ])
}
GROUP BY
  constraint_name,
  table_name,
  table_schema,
  ref_table,
  ref_table_table_schema,
  on_update,
  on_delete;
  `;
  },
  getTableColumnsSql: ({ name, schema }: QualifiedTable) => {
    if (!name || !schema) throw Error('empty parameters are not allowed!');

    return `not implemented`;
  },
};
