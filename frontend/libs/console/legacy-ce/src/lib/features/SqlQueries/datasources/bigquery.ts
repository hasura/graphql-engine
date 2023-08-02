import type { TableORSchemaArg } from '../../../dataSources/types';
import { QualifiedTable } from '../../../metadata/types';
import type { DatasourceSqlQueries } from '.';

export const bigquerySqlQueries: DatasourceSqlQueries = {
  getFetchTablesListQuery(options: TableORSchemaArg): string {
    let datasets = [];
    if ('schemas' in options) {
      datasets = options.schemas;
    } else {
      datasets = options.tables.map(t => t.schema) ?? [];
    }

    const query = (dataset: string) => `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_table
    select
    t.table_schema as table_schema,
    t.table_name as table_name,
    t.table_type as table_type, 
    opts.option_value as comment,
    CONCAT("[", c.json_data ,"]") as columns  
    FROM ${dataset}.INFORMATION_SCHEMA.TABLES as t
    LEFT JOIN 
    (
    with x as (
        select table_name, table_schema, column_name, ordinal_position, is_nullable, data_type from ${dataset}.INFORMATION_SCHEMA.COLUMNS
    ) select x.table_name as table_name, x.table_schema as table_schema, STRING_AGG(TO_JSON_STRING(x)) as json_data from x group by x.table_name,x.table_schema
    ) as c
    ON c.table_name = t.table_name and t.table_schema = c.table_schema
    LEFT JOIN ${dataset}.INFORMATION_SCHEMA.TABLE_OPTIONS as opts
    ON opts.table_name = t.table_name and opts.table_schema = t.table_schema and opts.option_name = "description"
  `;

    return datasets
      .map(dataset => {
        return query(dataset);
      })
      .join('union all');
  },
  primaryKeysInfoSql(options): string {
    return `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_primary_key
    select []`;
  },
  uniqueKeysSql(options): string {
    return `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_primary_key
    select []`;
  },
  checkConstraintsSql(options): string {
    return `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_check_constraint
    select []`;
  },
  getFKRelations(options): string {
    return `
    -- test_id = ${'schemas' in options ? 'multi' : 'single'}_foreign_key
    select []`;
  },
  getTableColumnsSql(table: QualifiedTable): string {
    const { name } = table;
    const schemaName = table?.dataset || table.schema;
    if (!schemaName || !name) throw Error('empty parameters are not allowed!');

    return `SELECT * FROM ${schemaName}.INFORMATION_SCHEMA.COLUMNS WHERE table_name = '${name}';`;
  },
};
