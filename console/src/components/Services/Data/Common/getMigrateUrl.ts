import Endpoints from '../../../../Endpoints';
import globals from '../../../../Globals';

import { CLI_CONSOLE_MODE } from '../../../../constants';

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

const returnMigrateUrl = (migrationMode: boolean, upQueries?: Query[]) => {
  if (globals.consoleMode === CLI_CONSOLE_MODE) {
    return migrationMode
      ? Endpoints.hasuractlMigrate
      : Endpoints.hasuractlMetadata;
  }
  if (!upQueries) {
    return Endpoints.query;
  }

  let endpoint = Endpoints.metadata;
  upQueries.forEach(query => {
    let type = '';
    if (query.type === 'bulk') {
      type = query.args[0].type;
    } else {
      type = query.type;
    }
    if (rqlQueryTypes.includes(type)) {
      endpoint = Endpoints.query;
    }
  });

  return endpoint;
};

export default returnMigrateUrl;
