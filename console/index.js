// import GraphiQL parts
import GraphiQLWrapper from './src/components/ApiExplorer/GraphiQLWrapper';

// import Data Tab parts
import dataRouter from './src/components/Services/Data/DataRouter';
import { dataReducer } from './src/components/Services/Data';
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

// import Event Tab parts
import eventRouter from './src/components/Services/EventTrigger/EventRouter';
import { eventReducer } from './src/components/Services/EventTrigger';

// import Remote Schema parts
import getCustomResolverRouter from './src/components/Services/CustomResolver/CustomResolverRouter.js';
import customResolverReducer from './src/components/Services/CustomResolver/customResolverReducer.js';

// import other globals
import routes from './src/routes';
import globals from './src/Globals';
import endpoints from './src/Endpoints';
import mainState from './src/components/Main/State';
import { changeRequestHeader } from './src/components/ApiExplorer/Actions';
import { validateLogin } from './src/components/Main/Actions';

const filterQueryScss = require('./src/components/Services/Data/TableBrowseRows/FilterQuery.scss');
const tableScss = require('./src/components/Services/Data/TableCommon/Table.scss');

// export GraphiQL parts
export { GraphiQLWrapper };

// export Data Tab parts
export { dataRouter, dataReducer };
export { fetchSchemaList, loadSchema, loadUntrackedSchema };
export { UPDATE_CURRENT_SCHEMA, UPDATE_DATA_HEADERS, ACCESS_KEY_ERROR };
export { dataHeaders };

// export Event Tab parts
export { eventRouter, eventReducer };

// export Remote Schema parts
export { getCustomResolverRouter, customResolverReducer };

// export other globals
export default routes;
export { globals, endpoints, mainState };
export { changeRequestHeader };
export { validateLogin };
export { handleMigrationErrors };

// export styles
export { filterQueryScss, tableScss };
