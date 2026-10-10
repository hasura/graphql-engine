import { runDatabaseMigration } from '@hasura/metadata/api';
import { ChangeDatabaseSchemaProps } from '../../types';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { getDropSchemaSql } from '../sqlQueries';

export const deleteDatabaseSchemaCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    schemaName,
    fetchJson,
    endpoints,
    isMigration,
    cascade,
  }: ChangeDatabaseSchemaProps) => {
    const source = { name: dataSourceName, kind };
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_schema_${schemaName}`,
        up: [
          {
            sql: getDropSchemaSql(schemaName, cascade),
          },
        ],
        down: [],
      },
    }).then((result) => result[0]);
  };
};
