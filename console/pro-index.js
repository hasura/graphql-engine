// import GraphiQL parts
import {
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
  // HASURA_CONSOLE_GRAPHIQL_HEADERS,
  // setGraphiQLHeadersInLocalStorage,
  // getGraphiQLHeadersFromLocalStorage,
} from './src/components/Services/ApiExplorer/ApiRequest/utils';

import {
  fetchConsoleNotifications
} from './src/components/Main/Actions';

import NotificationSection from './src/components/Main/NotificationSection';

import GraphiQLWrapper from './src/components/Services/ApiExplorer/GraphiQLWrapper/GraphiQLWrapper';

import { CONSOLE_ADMIN_SECRET } from './src/components/AppState';

// import utility functions

import { loadMigrationStatus } from './src/components/Main/Actions';

import {
  showErrorNotification,
  showSuccessNotification,
  showInfoNotification,
  showWarningNotification,
} from './src/components/Services/Common/Notification';

// Import main reducer

import mainReducer from './src/components/Main/Actions';

// Export main reducer

export {
  mainReducer,
};

// import Data Tab parts
import { dataRouterUtils } from './src/components/Services/Data/';
import { dataReducer } from './src/components/Services/Data';
import dataHeaders from './src/components/Services/Data/Common/Headers';
import { handleMigrationErrors } from './src/components/Services/Data/TableModify/ModifyActions';
import {
  fetchSchemaList,
  updateSchemaInfo,
  UPDATE_CURRENT_SCHEMA,
  UPDATE_DATA_HEADERS,
  ADMIN_SECRET_ERROR,
} from './src/components/Services/Data/DataActions';

// import Event Tab parts
import { getEventsRouter, eventsReducer } from './src/components/Services/Events';
// import { eventReducer } from './src/components/Services/EventTrigger';

// import Remote Schema parts
import {
  getRemoteSchemaRouter,
  remoteSchemaReducer,
} from './src/components/Services/RemoteSchema';

// Api Explorer and other exports */

import generatedApiExplorer from './src/components/Services/ApiExplorer/ApiExplorer';
import apiExplorerReducer from './src/components/Services/ApiExplorer/Actions';

import generatedLoginConnector from './src/components/Login/Login';

import generatedVoyagerConnector from './src/components/Services/VoyagerView/VoyagerView';
//

import telemetryReducer from './src/telemetry/Actions';

// Common components

// React components
import Spinner from './src/components/Common/Spinner/Spinner';

import invokeEventTriggerReducer from './src/components/Services/Events/EventTriggers/InvokeManualTrigger/InvokeManualTriggerAction';

// import Button from './src/components/Common/Button/Button';
// Css modules
const CommonScss = require('./src/components/Common/Common.scss');

export { Spinner, CommonScss };

// Meta data

import metadataContainer from './src/components/Services/Settings/Container';
import metadataOptionsContainer from './src/components/Services/Settings/MetadataOptions/MetadataOptions';
import metadataStatusContainer from './src/components/Services/Settings/MetadataStatus/MetadataStatus';
import allowedQueriesContainer from './src/components/Services/Settings/AllowedQueries/AllowedQueries';
import logoutContainer from './src/components/Services/Settings/Logout/Logout';
import aboutContainer from './src/components/Services/Settings/About/About';

import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
  isMetadataStatusPage,
} from './src/components/Services/Settings/Actions';

import { metadataReducer } from './src/components/Services/Settings/Actions';

// import other globals
// import routes from './src/routes';
import globals from './src/Globals';
import endpoints from './src/Endpoints';
import mainState from './src/components/Main/State';
import {
  changeRequestHeader,
  removeRequestHeader,
} from './src/components/Services/ApiExplorer/Actions';

import actionReducer from './src/components/Services/Actions/reducer';
import getActionsRouter from './src/components/Services/Actions/Router';
import typesReducer from './src/components/Services/Types/reducer';

import validateLogin from './src/utils/validateLogin';

import DragFoldTable from './src/components/Common/TableCommon/DragFoldTable';

const filterQueryScss = require('./src/components/Common/FilterQuery/FilterQuery.scss');
const tableScss = require('./src/components/Common/TableCommon/Table.scss');

// Export telemetry stuff

export { telemetryReducer };

// Export APIExplorer and entrypoints
export {
  // HASURA_CONSOLE_GRAPHIQL_HEADERS,
  // setGraphiQLHeadersInLocalStorage,
  // getGraphiQLHeadersFromLocalStorage,
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
  generatedApiExplorer,
  apiExplorerReducer,
  generatedLoginConnector,
  generatedVoyagerConnector,
};

// Export the metadata parts
export {
  metadataReducer,
  metadataContainer,
  metadataOptionsContainer,
  metadataStatusContainer,
  allowedQueriesContainer,
  loadInconsistentObjects,
  redirectToMetadataStatus,
  isMetadataStatusPage,
  logoutContainer,
  aboutContainer,
};

// export GraphiQL parts
export { GraphiQLWrapper };

// export Data Tab parts
export { dataRouterUtils, dataReducer };
export { fetchSchemaList, updateSchemaInfo };
export { ADMIN_SECRET_ERROR, UPDATE_CURRENT_SCHEMA, UPDATE_DATA_HEADERS };
export { dataHeaders };

// export Event Tab parts
export { getEventsRouter, eventsReducer, invokeEventTriggerReducer};

// export Remote Schema parts
export { getRemoteSchemaRouter, remoteSchemaReducer };

// export other globals

export { globals, endpoints, mainState };
export { changeRequestHeader, loadMigrationStatus, removeRequestHeader };
export { validateLogin };
export { handleMigrationErrors };

/* Export notification utilities */
export {
  showErrorNotification,
  showSuccessNotification,
  showInfoNotification,
  showWarningNotification,
};

export { DragFoldTable };

export { CONSOLE_ADMIN_SECRET };

// export styles
export { filterQueryScss, tableScss };
export * from './src/components/Common';
export { actionReducer, getActionsRouter, typesReducer };

/* Notification section exports
 */

export {
  fetchConsoleNotifications,
  NotificationSection,
}
