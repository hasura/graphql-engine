import { combineReducers } from 'redux';

import listReducer from './Actions';
import addReducer from './Add/addRemoteSchemaReducer';
import headerReducer from '../../Common/Layout/ReusableHeader/HeaderReducer';
import permissionsReducer from './Permissions/Actions';

const remoteSchemaReducer = combineReducers({
  addData: addReducer,
  listData: listReducer,
  headerData: headerReducer('REMOTE_SCHEMA', [
    {
      name: '',
      type: 'static',
      value: '',
    },
  ]),
  permissions: permissionsReducer,
});

export default remoteSchemaReducer;
