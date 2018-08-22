import Endpoints from '../../../../Endpoints';
import globals from '../../../../Globals';

const returnMigrateUrl = mode => {
  if (globals.consoleMode === 'cli') {
    return mode ? Endpoints.hasuractlMigrate : Endpoints.hasuractlMetadata;
  } else if (globals.consoleMode === 'hasuradb') {
    let finalUrl;
    if (globals.nodeEnv === 'development') {
      finalUrl = globals.devDataApiUrl + '/v1/query';
    } else {
      finalUrl = Endpoints.query;
    }
    return finalUrl;
  }
};

export default returnMigrateUrl;
