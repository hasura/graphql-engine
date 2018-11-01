import { combineReducers } from 'redux';

import listReducer from './customActions';
import addReducer from './Add/addResolverReducer';

const customResolverReducer = combineReducers({
  addData: addReducer,
  listData: listReducer,
});

export default customResolverReducer;
