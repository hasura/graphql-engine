import { combineReducers } from 'redux';

import listReducer from './Actions';
import addReducer from './Add/addRemoteSchemaReducer';

const remoteSchemaReducer = combineReducers({
  addData: addReducer,
  listData: listReducer,
});

export default remoteSchemaReducer;
