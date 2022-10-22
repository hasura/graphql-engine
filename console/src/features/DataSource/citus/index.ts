import { Database, Feature } from '..';
import { runSQL } from '../api';
import { adaptIntrospectedTables } from '../common/utils';
import { getTableColumns } from './introspection';

export type CitusTable = { name: string; schema: string };

export const citus: Database = {
  introspection: {
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getTrackableTables: async (dataSourceName: string) => {
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
        info_schema.table_schema NOT IN ('pg_catalog', 'citus', 'information_schema', 'columnar', 'guest', 'INFORMATION_SCHEMA', 'sys', 'db_owner', 'db_securityadmin', 'db_accessadmin', 'db_backupoperator', 'db_ddladmin', 'db_datawriter', 'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog', '_timescaledb_internal')
        AND NOT (info_schema.table_name = ANY (partitions.names)) 
        AND info_schema.table_name NOT IN ('citus_tables')    
      `;
      const tables = await runSQL({
        source: {
          name: dataSourceName,
          kind: 'citus',
        },
        sql,
      });

      return adaptIntrospectedTables(tables);
    },
    getDatabaseHierarchy: async () => {
      return ['schema', 'name'];
    },
    getTableColumns,
  },
};
