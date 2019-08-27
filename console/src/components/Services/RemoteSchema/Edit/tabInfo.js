import { REMOTE_SCHEMA_PERMISSIONS_SUPPORT } from '../../../../helpers/versionUtils'; 
import globals from '../../../../Globals';

const tabInfo = {
  details: {
    display_text: 'Details',
  },
  modify: {
    display_text: 'Modify',
  },
};

if (globals.featuresCompatibility && globals.featuresCompatibility[REMOTE_SCHEMA_PERMISSIONS_SUPPORT]) {
  tabInfo.permissions = {
    display_text: 'Permissions',
  }
}

export default tabInfo;
