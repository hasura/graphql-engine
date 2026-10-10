import { runSQL } from '@hasura/metadata/api';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { GetTableIndexesProps, TableIndex } from '../../types';
import { getTableIndexesSql } from '../sqlQueries';
import { adaptIndexes } from './adaptIndexes';

export const getTableIndexesCurry =
  (kind: PostgresFamilyDriver) =>
  async ({
    dataSourceName,
    table,
    fetchJson,
    endpoints,
  }: GetTableIndexesProps): Promise<TableIndex[]> => {
    const response = await runSQL({
      args: {
        source: { name: dataSourceName, kind },
        sql: getTableIndexesSql(table),
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });
    return adaptIndexes(response.result);
  };

export const getTableIndexes = getTableIndexesCurry('postgres');
