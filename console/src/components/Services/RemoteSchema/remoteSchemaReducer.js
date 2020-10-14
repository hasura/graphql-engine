import { combineReducers } from 'redux';

import listReducer from './Actions';
import addReducer from './Add/addRemoteSchemaReducer';
import permissionsReducer from './Permissions/reducer';
import headerReducer from '../../Common/Layout/ReusableHeader/HeaderReducer';

// common state
const commonState = {
  remoteSchemas: [],
  currentRemoteSchema: '',
  isFetching: false,
  error: null,
};

// common actions
export const LOADING_REMOTESCHEMAS = 'RemoteSchemas/LOAD_REMOTESCHEMAS';
export const LOADING_REMOTESCHEMAS_SUCCESS =
  'RemoteSchemas/LOADING_REMOTESCHEMAS_SUCCESS';
export const LOADING_REMOTESCHEMAS_FAILURE =
  'RemoteSchemas/LOADING_REMOTESCHEMAS_FAILURE';
export const setRemoteSchemas = remoteSchemas => ({
  type: LOADING_REMOTESCHEMAS_SUCCESS,
  data: remoteSchemas,
});
const SET_CURRENT_REMOTESCHEMA = 'RemoteSchemas/SET_CURRENT_REMOTESCHEMA';
export const setCurrentRemoteSchema = remoteSchemaName => ({
  type: SET_CURRENT_REMOTESCHEMA,
  name: remoteSchemaName,
});

// common reducer
const commonReducer = (state = commonState, action) => {
  switch (action.type) {
    case LOADING_REMOTESCHEMAS:
      return {
        ...state,
        isFetching: true,
      };
    case LOADING_REMOTESCHEMAS_SUCCESS:
      return {
        ...state,
        isFetching: false,
        remoteSchemas: action.data,
      };
    case LOADING_REMOTESCHEMAS_FAILURE:
      return {
        ...state,
        isFetching: false,
        error: action.error,
      };
    case SET_CURRENT_REMOTESCHEMA:
      return {
        ...state,
        currentRemoteSchema: action.name,
      };
    default:
      return state;
  }
};

// remote schema root reducer
const remoteSchemaReducer = combineReducers({
  common: commonReducer,
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
