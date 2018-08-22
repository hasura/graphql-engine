import tableReducer from './EventActions';
import addTableReducer from './Add/AddActions';

const eventReducer = {
  triggers: tableReducer,
  addTrigger: addTableReducer,
};

export default eventReducer;
