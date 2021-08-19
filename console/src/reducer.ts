import { combineReducers } from 'redux';
import { reducer as notifications } from 'react-notification-system-redux';
import { routerReducer } from 'react-router-redux';

import dataReducer from './components/Services/Data/DataReducer';
import { remoteSchemaReducer } from './components/Services/RemoteSchema';
import { actionsReducer } from './components/Services/Actions';
import { typesReducer } from './components/Services/Types';
import { eventsReducer } from './components/Services/Events';
import invokeEventTriggerReducer from './components/Services/Events/EventTriggers/InvokeManualTrigger/InvokeManualTriggerAction';
import mainReducer from './components/Main/Actions';
import apiExplorerReducer from './components/Services/ApiExplorer/Actions';
import progressBarReducer from './components/App/Actions';
import telemetryReducer from './telemetry/Actions';
import { metadataReducer } from './metadata/reducer';

const reducer = combineReducers({
  ...dataReducer,
  remoteSchemas: remoteSchemaReducer,
  actions: actionsReducer,
  progressBar: progressBarReducer,
  apiexplorer: apiExplorerReducer,
  main: mainReducer,
  routing: routerReducer,
  telemetry: telemetryReducer,
  notifications,
  metadata: metadataReducer,
  types: typesReducer,
  events: eventsReducer,
  invokeEventTrigger: invokeEventTriggerReducer,
});

export default reducer;
