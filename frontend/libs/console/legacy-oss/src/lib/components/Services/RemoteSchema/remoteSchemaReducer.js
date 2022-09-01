import { combineReducers } from 'redux';

import listReducer from './Actions';
import addReducer from './Add/addRemoteSchemaReducer';
import permissionsReducer from './Permissions/reducer';
import headerReducer from '../../Common/Layout/ReusableHeader/HeaderReducer';

const remoteSchemaReducer = combineReducers({
  addData: addReducer,
  listData: listReducer,
  permissions: permissionsReducer,
  headerData: headerReducer('REMOTE_SCHEMA', [
    {
      name: '',
      type: 'static',
      value: '',
    },
  ]),
});

export default remoteSchemaReducer;
