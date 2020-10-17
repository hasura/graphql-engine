// import GraphiQL parts
import {
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
} from './src/components/Services/ApiExplorer/ApiRequest/utils';

import {
  fetchConsoleNotifications
} from './src/components/Main/Actions';

import NotificationSection from './src/components/Main/NotificationSection';

export { default as PageNotFound } from './src/components/Error/PageNotFound';

import { CONSOLE_ADMIN_SECRET } from './src/components/AppState';

// import utility functions

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
// import { getEventsRouter, eventsReducer } from './src/components/Services/Events';
// import { eventReducer } from './src/components/Services/EventTrigger';

// import Remote Schema parts
// import {
//   getRemoteSchemaRouter,
//   remoteSchemaReducer,
// } from './src/components/Services/RemoteSchema';

// Api Explorer and other exports */

// import generatedApiExplorer from './src/components/Services/ApiExplorer/ApiExplorer';
// import apiExplorerReducer from './src/components/Services/ApiExplorer/Actions';
import generatedVoyagerConnector from './src/components/Services/VoyagerView/VoyagerView';

// import telemetryReducer from './src/telemetry/Actions';

// Common components

// React components
import Spinner from './src/components/Common/Spinner/Spinner';

// import invokeEventTriggerReducer from './src/components/Services/Events/EventTriggers/InvokeManualTrigger/InvokeManualTriggerAction';

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

// import other globals
// import routes from './src/routes';
import globals from './src/Globals';
import endpoints from './src/Endpoints';
import mainState from './src/components/Main/State';
import {
  changeRequestHeader,
  removeRequestHeader,
} from './src/components/Services/ApiExplorer/Actions';

const filterQueryScss = require('./src/components/Common/FilterQuery/FilterQuery.scss');
const tableScss = require('./src/components/Common/TableCommon/Table.scss');


// Export APIExplorer and entrypoints
export {
  // HASURA_CONSOLE_GRAPHIQL_HEADERS,
  // setGraphiQLHeadersInLocalStorage,
  // getGraphiQLHeadersFromLocalStorage,
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
  generatedVoyagerConnector,
};

// Export the metadata parts
export {
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

export { fetchSchemaList, updateSchemaInfo };
export { ADMIN_SECRET_ERROR, UPDATE_CURRENT_SCHEMA, UPDATE_DATA_HEADERS };
export { dataHeaders };

// export other globals

export { globals, endpoints, mainState };
export { changeRequestHeader, removeRequestHeader };
export { handleMigrationErrors };

export { CONSOLE_ADMIN_SECRET };

// export styles
export { filterQueryScss, tableScss };
export * from './src/components/Common';

/* Notification section exports
 */

export {
  fetchConsoleNotifications,
  NotificationSection,
}

export {
  loadConsoleOpts
} from './src/telemetry/Actions';
export * from './src/telemetry';


import * as EndpointNamedExps from './src/Endpoints';

export { default as Endpoints } from './src/Endpoints';
export { EndpointNamedExps };


