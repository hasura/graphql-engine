import Endpoints from '../../../../Endpoints';
import globals from '../../../../Globals';

import { SERVER_CONSOLE_MODE } from '../../../../constants';

const returnMigrateUrl = mode => {
  if (globals.consoleMode === 'cli') {
    return mode ? Endpoints.hasuractlMigrate : Endpoints.hasuractlMetadata;
  } else if (globals.consoleMode === SERVER_CONSOLE_MODE) {
    return Endpoints.query;
  }
};

export default returnMigrateUrl;
