/* */
import Endpoints from '../../../../../Endpoints';
import requestAction from '../../../../../utils/requestAction';
import dataHeaders from '../../../Data/Common/Headers';
import defaultState from './State';
/* */

/* */
const INVOKING_EVENT_TRIGGER = '@invokeManualTrigger/INVOKING_EVENT_TRIGGER';
const INVOKE_SUCCESS = '@invokeManualTrigger/INVOKE_SUCCESS';
const INVOKE_FAIL = '@invokeManualTrigger/INVOKE_FAIL';
const FETCHING_EVENT_STATUS = '@invokeManualTrigger/EVENT_STATUS';
const FETCH_EVENT_STATUS_SUCCESS =
  '@invokeManualTrigger/FETCH_EVENT_STATUS_SUCCESS';
const FETCH_EVENT_STATUS_FAIL = '@invokeManualTrigger/FETCH_EVENT_STATUS_FAIL';
const RESET = '@invokeManualTrigger/RESET';
/* */

/* */

const invokeManualTrigger = (name, args) => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const manualTriggerObj = {
    type: 'invoke_event_trigger',
    args: {
      name: name,
      payload: {
        new: args,
        old: null,
      },
    },
  };
  dispatch({ type: INVOKING_EVENT_TRIGGER });
  const options = {
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(manualTriggerObj),
  };
  return dispatch(requestAction(url, options))
    .then(data => {
      dispatch({ type: INVOKE_SUCCESS, data: data });
      return Promise.resolve(data);
    })
    .catch(err => {
      dispatch({ type: INVOKE_FAIL, data: err });
      return Promise.reject(err);
    });
};

const loadEventInvocations = eventId => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'event_invocation_logs',
          schema: 'hdb_catalog',
        },
        columns: [
          '*',
          {
            name: 'event',
            columns: ['*'],
          },
        ],
        where: {
          event_id: eventId,
        },
        order_by: ['-created_at'],
      },
    }),
  };
  dispatch({ type: FETCHING_EVENT_STATUS });
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: FETCH_EVENT_STATUS_SUCCESS, data: data });
      return Promise.resolve(data);
    },
    err => {
      dispatch({ type: FETCH_EVENT_STATUS_FAIL, data: err });
      return Promise.reject(err);
    }
  );
};

/* */

/* */
const invokeManualTriggerReducer = (state = defaultState, action) => {
  switch (action.type) {
    case INVOKING_EVENT_TRIGGER:
      return {
        ...state,
        isCreatingManualTrigger: true,
        success: {},
        err: null,
        status: [],
      };
    case INVOKE_SUCCESS:
      return {
        ...state,
        isCreatingManualTrigger: false,
        success: action.data,
      };
    case INVOKE_FAIL:
      return {
        ...state,
        isCreatingManualTrigger: false,
        err: action.data,
      };
    case FETCHING_EVENT_STATUS:
      return {
        ...state,
        isStatusFetching: true,
        status: [],
        statusFetchingErr: {},
      };
    case FETCH_EVENT_STATUS_SUCCESS:
      return {
        ...state,
        isStatusFetching: false,
        status: action.data,
      };
    case FETCH_EVENT_STATUS_FAIL:
      return {
        ...state,
        isStatusFetching: false,
        statusFetchingErr: action.data,
      };
    case RESET:
      return {
        ...defaultState,
      };
    default:
      return {
        ...state,
      };
  }
};

export { invokeManualTrigger, loadEventInvocations, RESET };

export default invokeManualTriggerReducer;
