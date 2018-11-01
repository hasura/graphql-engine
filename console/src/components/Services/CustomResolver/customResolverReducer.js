import { combineReducers } from 'redux';

import listReducer from './customActions';

const customResolverReducer = combineReducers({
  addData: () => {
    return { Hello: 'World' };
  },
  listData: listReducer,
});

export default customResolverReducer;
