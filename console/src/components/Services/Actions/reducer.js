import { combineReducers } from 'redux';

import addReducer from './Add/reducer';
import modifyReducer from './Modify/reducer';
import permissionsReducer from './Permissions/reducer';
import relationshipsReducer from './Relationships/reducer';
import typesReducer from './Types/reducer';

// common state
const commonState = {
  actions: [],
  currentAction: '',
  isFetching: false,
  error: null,
};

// common actions
export const LOADING_ACTIONS = 'Actions/LOAD_ACTIONS';
export const LOADING_ACTIONS_SUCCESS = 'Actions/LOADING_ACTIONS_SUCCESS';
export const LOADING_ACTIONS_FAILURE = 'Actions/LOADING_ACTIONS_FAILURE';
export const setActions = actions => ({
  type: LOADING_ACTIONS_SUCCESS,
  data: actions,
});
const SET_CURRENT_ACTION = 'Actions/SET_CURRENT_ACTION';
export const setCurrentAction = actionName => ({
  type: SET_CURRENT_ACTION,
  name: actionName,
});

// common reducer
const commonReducer = (state = commonState, action) => {
  switch (action.type) {
    case LOADING_ACTIONS:
      return {
        ...state,
        isFetching: true,
      };
    case LOADING_ACTIONS_SUCCESS:
      return {
        ...state,
        isFetching: false,
        actions: action.data,
      };
    case LOADING_ACTIONS_FAILURE:
      return {
        ...state,
        isFetching: false,
        error: action.error,
      };
    case SET_CURRENT_ACTION:
      return {
        ...state,
        currentAction: action.name,
      };
    default:
      return state;
  }
};

// actions root reducer
const reducer = combineReducers({
  add: addReducer,
  common: commonReducer,
  modify: modifyReducer,
  permissions: permissionsReducer,
  relationships: relationshipsReducer,
  types: typesReducer,
});

export default reducer;
