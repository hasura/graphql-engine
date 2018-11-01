import { combineReducers } from 'redux';

const customResolverReducer = combineReducers({
  addData: () => {
    return { Hello: 'World' };
  },
  listData: () => {
    return { Hello: 'World' };
  },
});

export default customResolverReducer;
