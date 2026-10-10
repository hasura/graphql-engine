import type { RunSQLResponse } from '@hasura/shared/types';
import { TableTrigger } from '../../types';
import { sqlResultToRows } from './sqlResultRows';

type TriggerColumn =
  | 'trigger_name'
  | 'action_timing'
  | 'events'
  | 'action_statement'
  | 'trigger_definition';

/** Parse the rows of getTableTriggersSql. Pure => cycle-free. */
export const adaptTriggers = (
  result: RunSQLResponse['result'] | undefined,
): TableTrigger[] =>
  sqlResultToRows<TriggerColumn>(result)
    .filter((row) => row.trigger_name)
    .map((row) => ({
      name: row.trigger_name as string,
      timing: row.action_timing,
      events: row.events,
      definition: row.action_statement,
      createStatement: row.trigger_definition,
    }));
