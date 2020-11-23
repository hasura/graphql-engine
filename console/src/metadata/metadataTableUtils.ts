import { getRunSqlQuery } from '../components/Common/utils/v1QueryUtils';
import { TriggerOperation } from '../components/Common/FilterQuery/state';

const eventRelTable = `"hdb_catalog"."event_log"`;
const eventInvTable = `"hdb_catalog"."event_invocation_logs"`;

export const getDataTriggerLogsQuery = (
  triggerOp: TriggerOperation,
  currentSource: string,
  triggerName: string,
  limit?: number,
  offset?: number
) => {
  let sql = '';

  if (triggerOp === 'pending') {
    sql = `SELECT original_table.*, data_table.*
    FROM ${eventInvTable} original_table JOIN ${eventRelTable} data_table
    ON original_table.event_id = data_table.id
    WHERE data_table.trigger_name = '${triggerName}' AND
    data_table.delivered = FALSE
    ORDER BY original_table.created_at DESC NULLS LAST`;
  }

  if (triggerOp === 'invocation' || triggerOp === 'processed') {
    // fixme: unsure of this, to check again.
    sql = `SELECT original_table.*, data_table.*
  FROM ${eventInvTable} original_table JOIN ${eventRelTable} data_table
  ON original_table.event_id = data_table.id
  WHERE data_table.trigger_name = '${triggerName}'
  ORDER BY original_table.created_at DESC NULLS LAST`;
  }

  if (limit) {
    sql += ` LIMIT ${limit}`;
  } else {
    sql += ` LIMIT 10`;
  }

  if (offset) {
    sql += ` OFFSET ${offset};`;
  } else {
    sql += ` OFFSET 0;`;
  }

  return getRunSqlQuery(sql, currentSource);
};
