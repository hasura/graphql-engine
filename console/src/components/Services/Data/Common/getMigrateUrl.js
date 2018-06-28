import Endpoints from '../../../../Endpoints';

const returnMigrateUrl = mode => {
  return mode ? Endpoints.hasuractlMigrate : Endpoints.hasuractlMetadata;
};

export default returnMigrateUrl;
