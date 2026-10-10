import { runSQL } from '@hasura/metadata/api';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import {
  GetTableTriggersProps,
  GetTriggerFunctionsProps,
  TableTrigger,
  TriggerFunction,
} from '../../types';
import { getTableTriggersSql, getTriggerFunctionsSql } from '../sqlQueries';
import { adaptTriggers } from './adaptTriggers';
import { sqlResultToRows } from './sqlResultRows';

export const getTableTriggersCurry =
  (kind: PostgresFamilyDriver) =>
  async ({
    dataSourceName,
    table,
    fetchJson,
    endpoints,
  }: GetTableTriggersProps): Promise<TableTrigger[]> => {
    const response = await runSQL({
      args: {
        source: { name: dataSourceName, kind },
        sql: getTableTriggersSql(table, {
          withDefinition: kind !== 'cockroach',
        }),
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });
    return adaptTriggers(response.result);
  };

export const getTriggerFunctionsCurry =
  (kind: PostgresFamilyDriver) =>
  async ({
    dataSourceName,
    fetchJson,
    endpoints,
  }: GetTriggerFunctionsProps): Promise<TriggerFunction[]> => {
    const response = await runSQL({
      args: {
        source: { name: dataSourceName, kind },
        sql: getTriggerFunctionsSql(),
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });
    return sqlResultToRows<'function_schema' | 'function_name'>(
      response.result,
    ).map((row) => ({
      schema: row.function_schema ?? '',
      name: row.function_name ?? '',
    }));
  };

export const getTableTriggers = getTableTriggersCurry('postgres');
