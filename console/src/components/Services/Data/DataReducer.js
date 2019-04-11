import { combineReducers } from 'redux';
import tableReducer from './DataActions';
import addTableReducer from './Add/AddActions';
import addExistingTableReducer from './Add/AddExistingTableViewActions';
import rawSQLReducer from './RawSQL/Actions';

import customFunctionReducer from './Function/customFunctionReducer';

const dataReducer = {
  tables: tableReducer,
  functions: customFunctionReducer,
  addTable: combineReducers({
    table: addTableReducer,
    existingTableView: addExistingTableReducer,
  }),
  rawSQL: rawSQLReducer,
};

export default dataReducer;
