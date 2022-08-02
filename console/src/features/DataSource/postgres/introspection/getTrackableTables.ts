import { runSQL } from '../../api';
import { adaptIntrospectedTables } from '../../common/utils';

export const getTrackableTables = async (dataSourceName: string) => {
  const sql = `
  WITH partitions as (
    SELECT array(
      SELECT
      child.relname       AS partition
  FROM pg_inherits
      JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
      JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
    ) as names
  )
  SELECT info_schema.table_name, info_schema.table_schema, info_schema.table_type 
  FROM information_schema.tables as info_schema, partitions
  WHERE 
    info_schema.table_schema NOT IN ('information_schema', 'pg_catalog', 'hdb_catalog', '_timescaledb_internal')
    AND NOT (info_schema.table_name = ANY (partitions.names))     
  `;

  const tables = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'postgres',
    },
    sql,
  });

  return adaptIntrospectedTables(tables);
};
