import { runDatabaseMigration } from '@hasura/metadata/api';
import { ChangeDatabaseSchemaProps } from '../../types';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { getCreateSchemaSql, getDropSchemaSql } from '../sqlQueries';

export const createDatabaseSchemaCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    schemaName,
    endpoints,
    fetchJson,
    isMigration,
  }: ChangeDatabaseSchemaProps) => {
    const source = { name: dataSourceName, kind };
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `create_schema_${schemaName}`,
        up: [
          {
            sql: getCreateSchemaSql(schemaName),
          },
        ],
        down: [
          {
            sql: getDropSchemaSql(schemaName),
          },
        ],
      },
    }).then((result) => result[0]);
  };
};
