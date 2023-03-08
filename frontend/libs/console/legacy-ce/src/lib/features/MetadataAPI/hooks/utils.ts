import { CLI_CONSOLE_MODE } from '../../../constants';
import Endpoints from '../../../Endpoints';
import globals from '../../../Globals';

const rqlQueryTypes = [
  'select',
  'count',
  'insert',
  'delete',
  'update',
  'run_sql',
  'mssql_run_sql',
  'citus_run_sql',
];

type Query = {
  type: string;
  args: Record<string, any>;
};

export const returnMigrateUrl = (
  migrationMode: boolean,
  upQueries?: Query[],
  overrideCliMode?: boolean
) => {
  if (globals.consoleMode === CLI_CONSOLE_MODE && !overrideCliMode) {
    return migrationMode
      ? Endpoints.hasuraCliServerMigrate
      : Endpoints.hasuraCliServerMetadata;
  }
  if (!upQueries) {
    return Endpoints.query;
  }

  let endpoint = Endpoints.metadata;
  upQueries.forEach(query => {
    let type = '';
    if (query.type === 'bulk' || query.type === 'concurrent_bulk') {
      type = query?.args?.[0]?.type;
    } else {
      type = query.type;
    }
    if (rqlQueryTypes.includes(type)) {
      endpoint = Endpoints.query;
    }
  });

  return endpoint;
};
