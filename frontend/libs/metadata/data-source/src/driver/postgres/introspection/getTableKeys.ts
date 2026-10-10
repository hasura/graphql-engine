import { runSQL } from '@hasura/metadata/api';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { GetTableKeysProps, TableKeyConstraint } from '../../types';
import { PostgresTable } from '../types';
import { getKeysSql } from '../sqlQueries';
import { adaptKeyConstraints } from './adaptKeys';

const runKeysQuery = async (
  kind: PostgresFamilyDriver,
  { dataSourceName, table, fetchJson, endpoints }: GetTableKeysProps,
  sql: string,
): Promise<TableKeyConstraint[]> => {
  const response = await runSQL({
    args: { source: { name: dataSourceName, kind }, sql, readOnly: true },
    fetchJson,
    url: endpoints.queryV2,
  });
  return adaptKeyConstraints(response.result);
};

export const getPrimaryKeyCurry =
  (kind: PostgresFamilyDriver) =>
  async (props: GetTableKeysProps): Promise<TableKeyConstraint | null> => {
    const { schema, name } = props.table as PostgresTable;
    const sql = getKeysSql('PRIMARY KEY', {
      tables: [{ name, schema }],
    });
    const keys = await runKeysQuery(kind, props, sql);
    return keys[0] ?? null;
  };

export const getUniqueKeysCurry =
  (kind: PostgresFamilyDriver) =>
  async (props: GetTableKeysProps): Promise<TableKeyConstraint[]> => {
    const { schema, name } = props.table as PostgresTable;
    const sql = getKeysSql('UNIQUE', {
      tables: [{ name, schema }],
    });
    return runKeysQuery(kind, props, sql);
  };

export const getPrimaryKey = getPrimaryKeyCurry('postgres');
export const getUniqueKeys = getUniqueKeysCurry('postgres');
