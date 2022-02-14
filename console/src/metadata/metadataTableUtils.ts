import { getRunSqlQuery } from '../components/Common/utils/v1QueryUtils';
import { TriggerOperation } from '../components/Common/FilterQuery/state';

const eventRelTable = `"hdb_catalog"."event_log"`;
const eventInvTable = `"hdb_catalog"."event_invocation_logs"`;

const triggerTypes = {
  pending: 'pending',
  processed: 'processed',
  invocation: 'invocation',
};

export interface RunSQLQueryType {
  type: string;
  args: {
    source: string;
    sql: string;
    cascade: boolean;
    read_only: boolean;
  };
}

export const getDataTriggerLogsCountQuery = (
  triggerName: string,
  triggerOp: TriggerOperation,
  currentSource?: string
): RunSQLQueryType => {
  let qry = `SELECT
	COUNT(*)
  FROM ${eventRelTable} data_table
  WHERE data_table.trigger_name = '${triggerName}' `;

  switch (triggerOp) {
    case triggerTypes.pending:
      qry += `AND delivered=false AND error=false AND archived=false;`;
      break;

    case triggerTypes.processed:
      qry += `AND (delivered=true OR error=true) AND archived=false;`;
      break;
    case triggerTypes.invocation:
      qry = `SELECT
        COUNT(*)
        FROM ${eventInvTable} original_table JOIN ${eventRelTable} data_table
        ON original_table.event_id = data_table.id
        WHERE data_table.trigger_name = '${triggerName}' `;
      break;
    default:
      break;
  }
  return getRunSqlQuery(qry, currentSource ?? 'default');
};
export const getDataTriggerLogsQuery = (
  triggerOp: TriggerOperation,
  currentSource: string,
  triggerName: string,
  limit?: number,
  offset?: number
): RunSQLQueryType => {
  let sql = '';

  switch (triggerOp) {
    case triggerTypes.pending:
      sql = `SELECT *
      FROM ${eventRelTable} data_table
      WHERE data_table.trigger_name = '${triggerName}'  
      AND delivered=false AND error=false AND archived=false ORDER BY created_at DESC `;
      break;

    case triggerTypes.processed:
      sql = `SELECT *
      FROM ${eventRelTable} data_table 
      WHERE data_table.trigger_name = '${triggerName}' 
      AND (delivered=true OR error=true) AND archived=false ORDER BY created_at DESC `;
      break;
    case triggerTypes.invocation:
      sql = `SELECT original_table.*, data_table.*
      FROM ${eventInvTable} original_table JOIN ${eventRelTable} data_table ON original_table.event_id = data_table.id
      WHERE data_table.trigger_name = '${triggerName}' 
      ORDER BY original_table.created_at DESC NULLS LAST`;
      break;
    default:
      break;
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
export const getDataTriggerInvocations = (
  eventId: string,
  currentSource = 'default'
): RunSQLQueryType => {
  const sql = `SELECT
      *
    FROM 
      ${eventInvTable}
    WHERE
      event_id = '${eventId}'
    ORDER BY
      created_at DESC NULLS LAST;`;

  return getRunSqlQuery(sql, currentSource);
};
