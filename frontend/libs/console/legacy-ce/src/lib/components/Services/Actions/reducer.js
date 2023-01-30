import { combineReducers } from 'redux';

import addReducer from './Add/reducer';
import modifyReducer from './Modify/reducer';
import permissionsReducer from './Permissions/reducer';
import relationshipsReducer from './Relationships/reducer';
import typesReducer from './Types/reducer';

// common state
const commonState = {
  currentAction: '',
  isFetching: false,
  error: null,
};

// common actions
const SET_CURRENT_ACTION = 'Actions/SET_CURRENT_ACTION';
export const setCurrentAction = actionName => ({
  type: SET_CURRENT_ACTION,
  name: actionName,
});

// common reducer
const commonReducer = (state = commonState, action) => {
  switch (action.type) {
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
