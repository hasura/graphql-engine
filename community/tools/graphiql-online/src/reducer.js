import { combineReducers } from 'redux';
import { routerReducer } from 'react-router-redux';
import apiExplorerReducer from 'components/ApiExplorer/Actions';
import progressBarReducer from 'components/App/Actions';

const reducer = combineReducers({
  progressBar: progressBarReducer,
  apiexplorer: apiExplorerReducer,
  routing: routerReducer,
});

export default reducer;
