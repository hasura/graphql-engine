// import GraphiQL parts
import {
  HASURA_CONSOLE_GRAPHIQL_HEADERS,
  setGraphiQLHeadersInLocalStorage,
  getGraphiQLHeadersFromLocalStorage,
} from './src/components/Services/ApiExplorer/ApiRequest/utils';

import GraphiQLWrapper from './src/components/Services/ApiExplorer/GraphiQLWrapper/GraphiQLWrapper';

// import utility functions

import { loadMigrationStatus } from './src/components/Main/Actions';

import {
  showErrorNotification,
  showSuccessNotification,
  showInfoNotification,
  showWarningNotification,
} from './src/components/Services/Common/Notification';

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
import { eventRouterUtils } from './src/components/Services/EventTrigger';
import { eventReducer } from './src/components/Services/EventTrigger';

// import Remote Schema parts
import {
  getRemoteSchemaRouter,
  remoteSchemaReducer,
} from './src/components/Services/RemoteSchema';

// Api Explorer and other exports */

import generatedApiExplorer from './src/components/Services/ApiExplorer/ApiExplorerGenerator';
import apiExplorerReducer from './src/components/Services/ApiExplorer/Actions';

import generatedLoginConnector from './src/components/Login/Login';

import generatedVoyagerConnector from './src/components/Services/VoyagerView/VoyagerView';
//

import telemetryReducer from './src/telemetry/Actions';

// Common components

// React components
import Spinner from './src/components/Common/Spinner/Spinner';
import Button from './src/components/Common/Button/Button';
// Css modules
const CommonScss = require('./src/components/Common/Common.scss');

export { Spinner, Button, CommonScss };

// Meta data

import metadataContainer from './src/components/Services/Metadata/Container';
import metadataOptionsContainer from './src/components/Services/Metadata/MetadataOptions/MetadataOptions';
import metadataStatusContainer from './src/components/Services/Metadata/MetadataStatus/MetadataStatus';
import allowedQueriesContainer from './src/components/Services/Metadata/AllowedQueries/AllowedQueries';

import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
  isMetadataStatusPage,
} from './src/components/Services/Metadata/Actions';

import { metadataReducer } from './src/components/Services/Metadata/Actions';

// import other globals
// import routes from './src/routes';
import globals from './src/Globals';
import endpoints from './src/Endpoints';
import mainState from './src/components/Main/State';
import { changeRequestHeader } from './src/components/Services/ApiExplorer/Actions';
import validateLogin from './src/utils/validateLogin';

const filterQueryScss = require('./src/components/Common/FilterQuery/FilterQuery.scss');
const tableScss = require('./src/components/Common/TableCommon/Table.scss');

// Export telemetry stuff

export { telemetryReducer };

// Export APIExplorer and entrypoints
export {
  HASURA_CONSOLE_GRAPHIQL_HEADERS,
  setGraphiQLHeadersInLocalStorage,
  getGraphiQLHeadersFromLocalStorage,
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
};

// export GraphiQL parts
export { GraphiQLWrapper };

// export Data Tab parts
export { dataRouterUtils, dataReducer };
export { fetchSchemaList, updateSchemaInfo };
export { ADMIN_SECRET_ERROR, UPDATE_CURRENT_SCHEMA, UPDATE_DATA_HEADERS };
export { dataHeaders };

// export Event Tab parts
export { eventRouterUtils, eventReducer };

// export Remote Schema parts
export { getRemoteSchemaRouter, remoteSchemaReducer };

// export other globals

export { globals, endpoints, mainState };
export { changeRequestHeader, loadMigrationStatus };
export { validateLogin };
export { handleMigrationErrors };

/* Export notification utilities */
export {
  showErrorNotification,
  showSuccessNotification,
  showInfoNotification,
  showWarningNotification,
};

// export styles
export { filterQueryScss, tableScss };
