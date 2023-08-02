import type { TableORSchemaArg } from '../../../dataSources/types';
import { QualifiedTable } from '../../../metadata/types';
import { getSchemasWhereClause, getTablesWhereClause } from './common';
import type { DatasourceSqlQueries } from '.';

const getSqlKey = (
  opts: TableORSchemaArg,
  check: 'is_unique_constraint' | 'is_primary_key'
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

export const msSqlQueries: DatasourceSqlQueries = {
  getFetchTablesListQuery(options: TableORSchemaArg): string {
    const whereClause =
      'schemas' in options
        ? getSchemasWhereClause(options.schemas, 'AND')('sch.name')
        : getTablesWhereClause(
            options.tables,
            'AND'
          )({
            name: 'obj.name',
            schema: 'sch.name',
          });
    return `
  -- test_id = ${'schemas' in options ? 'multi' : 'single'}_table
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
) AS [isc](json) where obj.type_desc in ('USER_TABLE', 'VIEW') 
AND (obj.name not in ('spt_fallback_db', 'spt_fallback_dev', 'spt_fallback_usg', 'spt_values', 'spt_monitor', 'MSreplication_options'))
${whereClause};`;
  },
  primaryKeysInfoSql(options: TableORSchemaArg): string {
    return getSqlKey(options, 'is_primary_key');
  },
  uniqueKeysSql(options: TableORSchemaArg): string {
    return getSqlKey(options, 'is_unique_constraint');
  },
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
  getFKRelations(options: TableORSchemaArg): string {
    return `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_foreign_key
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
      FROM
        sys.foreign_key_columns fkc
        INNER JOIN sys.columns col1 ON col1.column_id = fkc.parent_column_id
        AND col1.object_id = tab1.object_id
        INNER JOIN sys.columns col2 ON col2.column_id = fkc.referenced_column_id
        AND col2.object_id = tab2.object_id
      WHERE
        fk.object_id = fkc.constraint_object_id FOR JSON PATH
    ) AS column_mapping,
    fk.delete_referential_action_desc AS [on_delete],
    fk.update_referential_action_desc AS [on_update]
  FROM
    sys.foreign_keys fk
    INNER JOIN sys.objects obj ON obj.object_id = fk.referenced_object_id
    INNER JOIN sys.tables tab1 ON tab1.object_id = fk.parent_object_id
    INNER JOIN sys.schemas sch1 ON tab1.schema_id = sch1.schema_id
    INNER JOIN sys.tables tab2 ON tab2.object_id = fk.referenced_object_id
    INNER JOIN sys.schemas sch2 ON tab2.schema_id = sch2.schema_id
  ${
    'schemas' in options
      ? getSchemasWhereClause(options.schemas)(['sch1.name', 'sch2.name'])
      : getTablesWhereClause(options.tables)([
          { name: 'tab1.name', schema: 'sch1.name' },
          { name: 'tab2.name', schema: 'sch2.name' },
        ])
  }
  for json path; 
  `;
  },
  getTableColumnsSql: ({ name, schema }: QualifiedTable) => {
    if (!name || !schema) throw Error('empty parameters are not allowed!');

    return `SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'${name}' AND TABLE_SCHEMA= N'${schema}'`;
  },
};
