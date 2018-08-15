// import GraphiQL parts
import GraphiQLWrapper from './src/components/ApiExplorer/GraphiQLWrapper';

// import Data Tab parts
import routes from './src/routes';
import dataRouter from './src/components/Services/Data/DataRouter';
import { dataReducer } from './src/components/Services/Data';
import globals from './src/Globals';
import endpoints from './src/Endpoints';
import mainState from './src/components/Main/State';
import { changeRequestHeader } from './src/components/ApiExplorer/Actions';
import { validateLogin } from './src/components/Main/Actions';
import dataHeaders from './src/components/Services/Data/Common/Headers';
import { handleMigrationErrors } from './src/components/Services/Data/TableModify/ModifyActions';
import {
  fetchSchemaList,
  loadSchema,
  loadUntrackedSchema,
  UPDATE_CURRENT_SCHEMA,
  UPDATE_DATA_HEADERS,
  ACCESS_KEY_ERROR,
} from './src/components/Services/Data/DataActions';

const filterQueryScss = require('./src/components/Services/Data/TableBrowseRows/FilterQuery.scss');
const tableScss = require('./src/components/Services/Data/TableCommon/Table.scss');

// export GraphiQL parts
export { GraphiQLWrapper };

// export Data Tab parts
export default routes;
export { dataRouter, dataReducer };
export { globals, endpoints, mainState };
export { fetchSchemaList, loadSchema, loadUntrackedSchema };
export { UPDATE_CURRENT_SCHEMA, UPDATE_DATA_HEADERS, ACCESS_KEY_ERROR };
export { changeRequestHeader };
export { validateLogin };
export { dataHeaders };
export { filterQueryScss, tableScss };
export { handleMigrationErrors };
