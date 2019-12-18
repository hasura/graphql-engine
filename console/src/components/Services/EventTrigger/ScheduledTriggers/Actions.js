import endpoints from '../../../../Endpoints';
import requestAction from '../../../../utils/requestAction';

import { combineReducers } from 'redux';

import addScheduledTriggerReducer from './AddScheduledTriggerActions';

export const makeQuery = query => (dispatch, getState) => {
  const { dataHeaders } = getState().tables;

  const options = {
    method: 'POST',
    headers: {
      ...dataHeaders,
    },
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(endpoints.query, options));
};

export const fetchScheduledTriggers = () => {
  const query = {
    type: 'run_sql',
    args: {
      sql: 'select * from hdb_catalog.hdb_scheduled_trigger',
    },
  };
  return query;
};

export const deleteScheduledTriggersQuery = name => {
  if (!name) {
    throw new Error('Invalid trigger name');
  }
  const query = {
    type: 'run_sql',
    args: {
      sql: `Delete from hdb_catalog.hdb_scheduled_trigger where name='${name}'`,
    },
  };
  return query;
};

export const applyAllowList = exportList => (dispatch, getState) => {
  const { dataHeaders } = getState().tables;

  const query = {
    type: 'bulk',
    args: exportList,
  };

  const options = {
    method: 'POST',
    headers: {
      ...dataHeaders,
    },
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(endpoints.query, options));
};

const scheduledTriggerReducer = combineReducers({
  addScheduledTrigger: addScheduledTriggerReducer,
});
export default scheduledTriggerReducer;
