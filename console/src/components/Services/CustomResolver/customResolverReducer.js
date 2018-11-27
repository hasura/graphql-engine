import { combineReducers } from 'redux';

import listReducer from './customActions';
import addReducer from './Add/addResolverReducer';
import headerReducer from '../Layout/ReusableHeader/HeaderReducer';

const customResolverReducer = combineReducers({
  addData: addReducer,
  listData: listReducer,
  headerData: headerReducer('CUSTOM_RESOLVER', [
    {
      name: '',
      type: 'static',
      value: '',
    },
  ]),
});

export default customResolverReducer;
