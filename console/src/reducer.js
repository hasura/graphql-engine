import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';
import { dataReducer } from './components/Services/Data';
import { eventReducer } from './components/Services/EventTrigger';
import { remoteSchemaReducer } from './components/Services/RemoteSchema';
import { actionsReducer } from './components/Services/Actions';
import { typesReducer } from './components/Services/Types';
import { eventsReducer } from './components/Services/Events';
import mainReducer from './components/Main/Actions';
import apiExplorerReducer from 'components/Services/ApiExplorer/Actions';
import progressBarReducer from 'components/App/Actions';
import telemetryReducer from './telemetry/Actions';
import { metadataReducer } from './components/Services/Settings/Actions';

import { reducer as notifications } from 'react-notification-system-redux';

const reducer = combineReducers({
  ...dataReducer,
  ...eventReducer,
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
});

export default reducer;
