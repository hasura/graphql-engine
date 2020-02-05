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
      sql:
        'select name, webhook_conf, schedule, payload from hdb_catalog.hdb_scheduled_trigger',
    },
  };
  return query;
};

export const fetchPastInvocations = () => {
  const query = {
    type: 'run_sql',
    args: {
      sql:
        'select e.name, l.event_id, l.status, l.id, l.request, l.response, l.created_at from hdb_catalog.hdb_scheduled_event_invocation_logs l JOIN hdb_catalog.hdb_scheduled_events e ON e.id = l.event_id order by created_at desc limit 30;',
    },
  };
  return query;
};

export const fetchUpcomingEvents = () => {
  const query = {
    type: 'run_sql',
    args: {
      sql:
        'select name, scheduled_time, id from hdb_catalog.hdb_scheduled_events where delivered=false and error=false and scheduled_time > now() order by scheduled_time asc limit 30;',
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
      sql: `delete from hdb_catalog.hdb_scheduled_trigger where name='${name}'`,
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
