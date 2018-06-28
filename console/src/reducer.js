import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';
import { dataReducer } from './components/Services/Data';
import mainReducer from './components/Main/Actions';
import apiExplorerReducer from 'components/ApiExplorer/Actions';
import progressBarReducer from 'components/App/Actions';

import { reducer as notifications } from 'react-notification-system-redux';

const reducer = combineReducers({
  ...dataReducer,
  progressBar: progressBarReducer,
  apiexplorer: apiExplorerReducer,
  main: mainReducer,
  routing: routerReducer,
  notifications,
});

export default reducer;
