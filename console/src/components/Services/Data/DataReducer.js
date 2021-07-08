import { combineReducers } from 'redux';
import tableReducer from './DataActions';
import addTableReducer from './Add/AddActions';
import addExistingTableReducer from './Add/AddExistingTableViewActions';
import rawSQLReducer from './RawSQL/Actions';
import { dataSidebarReducer } from './DataSubSidebar';
import customFunctionReducer from './Function/customFunctionReducer';
import { schemaSharingReducer } from './Schema/SchemaSharing/Actions';

const dataReducer = {
  tables: tableReducer,
  functions: customFunctionReducer,
  schemaSharing: schemaSharingReducer,
  addTable: combineReducers({
    table: addTableReducer,
    existingTableView: addExistingTableReducer,
  }),
  rawSQL: rawSQLReducer,
  dataSidebar: dataSidebarReducer,
};

export default dataReducer;
