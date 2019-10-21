import { combineReducers } from 'redux';

import addReducer from './Add/reducer';
import modifyReducer from './Modify/reducer';
import permissionsReducer from './Permissions/reducer';

// list state
const listState = {
  actions: [],
  currentAction: '',
  isFetching: false,
  error: null,
};

// list actions
export const LOADING_ACTIONS = 'Actions/LOAD_ACTIONS';
export const LOADING_ACTIONS_SUCCESS = 'Actions/LOADING_ACTIONS_SUCCESS';
export const LOADING_ACTIONS_FAILURE = 'Actions/LOADING_ACTIONS_FAILURE';
export const setActions = actions => ({
  type: LOADING_ACTIONS_SUCCESS,
  data: actions,
});

// list reducer
const listingReducer = (state = listState, action) => {
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
    default:
      return state;
  }
};

// actions root reducer
const reducer = combineReducers({
  add: addReducer,
  list: listingReducer,
  modify: modifyReducer,
  permissions: permissionsReducer,
});

export default reducer;
