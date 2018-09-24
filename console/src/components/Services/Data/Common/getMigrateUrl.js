import Endpoints from '../../../../Endpoints';
import globals from '../../../../Globals';

const returnMigrateUrl = mode => {
  if (globals.consoleMode === 'cli') {
    return mode ? Endpoints.hasuractlMigrate : Endpoints.hasuractlMetadata;
  } else if (globals.consoleMode === 'hasuradb') {
    return Endpoints.query;
  }
};

export default returnMigrateUrl;
