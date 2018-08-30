import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import defaultState from './EventState';
import processedEventsReducer from './ProcessedEvents/ViewActions';
import pendingEventsReducer from './PendingEvents/ViewActions';
import { showErrorNotification, showSuccessNotification } from './Notification';
import dataHeaders from './Common/Headers';
import { loadMigrationStatus } from '../../Main/Actions';
import returnMigrateUrl from './Common/getMigrateUrl';
import globals from '../../../Globals';
import { push } from 'react-router-redux';

const SET_TRIGGER = 'Event/SET_TRIGGER';
const LOAD_TRIGGER_LIST = 'Event/LOAD_TRIGGER_LIST';
const LOAD_PROCESSED_EVENTS = 'Event/LOAD_PROCESSED_EVENTS';
const LOAD_PENDING_EVENTS = 'Event/LOAD_PENDING_EVENTS';
const ACCESS_KEY_ERROR = 'Event/ACCESS_KEY_ERROR';
const UPDATE_DATA_HEADERS = 'Event/UPDATE_DATA_HEADERS';
const LISTING_TRIGGER = 'Event/LISTING_TRIGGER';

const MAKE_REQUEST = 'Event/MAKE_REQUEST';
const REQUEST_SUCCESS = 'Event/REQUEST_SUCCESS';
const REQUEST_ERROR = 'Event/REQUEST_ERROR';

/* ************ action creators *********************** */
const loadTriggers = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'event_triggers',
          schema: 'hdb_catalog',
        },
        columns: [
          '*',
          {
            name: 'retry_conf',
            columns: ['*'],
          },
        ],
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_TRIGGER_LIST, triggerList: data });
    },
    error => {
      console.error('Failed to load triggers' + JSON.stringify(error));
    }
  );
};

const loadProcessedEvents = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'event_triggers',
          schema: 'hdb_catalog',
        },
        columns: [
          '*',
          {
            name: 'events',
            columns: [
              '*',
              { name: 'logs', columns: ['*'], order_by: ['-created_at'] },
            ],
            where: {
              $or: [{ delivered: { $eq: true } }, { error: { $eq: true } }],
            },
            order_by: ['-created_at'],
            limit: 10,
          },
        ],
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_PROCESSED_EVENTS, data: data });
    },
    error => {
      console.error('Failed to load triggers' + JSON.stringify(error));
    }
  );
};

const loadPendingEvents = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'select',
      args: {
        table: {
          name: 'event_triggers',
          schema: 'hdb_catalog',
        },
        columns: [
          '*',
          {
            name: 'events',
            columns: [
              '*',
              { name: 'logs', columns: ['*'], order_by: ['-created_at'] },
            ],
            where: { delivered: false, error: false },
            order_by: ['-created_at'],
            limit: 10,
          },
        ],
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_PENDING_EVENTS, data: data });
    },
    error => {
      console.error('Failed to load triggers' + JSON.stringify(error));
    }
  );
};

const setTrigger = triggerName => ({ type: SET_TRIGGER, triggerName });

/* **********Shared functions between table actions********* */

const handleMigrationErrors = (title, errorMsg) => dispatch => {
  const requestMsg = title;
  if (globals.consoleMode === 'hasuradb') {
    // handle errors for run_sql based workflow
    dispatch(showErrorNotification(title, errorMsg.code, requestMsg, errorMsg));
  } else if (errorMsg.code === 'migration_failed') {
    dispatch(
      showErrorNotification(title, 'Migration Failed', requestMsg, errorMsg)
    );
  } else if (errorMsg.code === 'data_api_error') {
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(
      showErrorNotification(
        title,
        parsedErrorMsg.message.error,
        requestMsg,
        parsedErrorMsg
      )
    );
  } else {
    // any other unhandled codes
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(
      showErrorNotification(title, errorMsg.code, requestMsg, parsedErrorMsg)
    );
  }
  // dispatch(showErrorNotification(msg, firstDisplay, request, response));
};

const makeMigrationCall = (
  dispatch,
  getState,
  upQueries,
  downQueries,
  migrationName,
  customOnSuccess,
  customOnError,
  requestMsg,
  successMsg,
  errorMsg
) => {
  const upQuery = {
    type: 'bulk',
    args: upQueries,
  };

  const downQuery = {
    type: 'bulk',
    args: downQueries,
  };

  const migrationBody = {
    name: migrationName,
    up: upQuery.args,
    down: downQuery.args,
  };

  const currMigrationMode = getState().main.migrationMode;

  const migrateUrl = returnMigrateUrl(currMigrationMode);

  let finalReqBody;
  if (globals.consoleMode === 'hasuradb') {
    finalReqBody = upQuery;
  } else if (globals.consoleMode === 'cli') {
    finalReqBody = migrationBody;
  }
  const url = migrateUrl;
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: dataHeaders(getState),
    body: JSON.stringify(finalReqBody),
  };

  const onSuccess = () => {
    if (globals.consoleMode === 'cli') {
      dispatch(loadMigrationStatus()); // don't call for hasuradb mode
    }
    dispatch(loadTriggers());
    customOnSuccess();
    if (successMsg) {
      dispatch(showSuccessNotification(successMsg));
    }
  };

  const onError = err => {
    customOnError(err);
    dispatch(handleMigrationErrors(errorMsg, err));
  };

  dispatch({ type: MAKE_REQUEST });
  dispatch(showSuccessNotification(requestMsg));
  dispatch(requestAction(url, options, REQUEST_SUCCESS, REQUEST_ERROR)).then(
    onSuccess,
    onError
  );
};

const deleteTrigger = triggerName => {
  return (dispatch, getState) => {
    dispatch(showSuccessNotification('Deleting Trigger...'));

    // apply migrations
    const migrationName = 'delete_trigger_' + triggerName.trim();
    const payload = {
      type: 'unsubscribe_table',
      args: {
        name: triggerName,
      },
    };
    const upQueryArgs = [];
    upQueryArgs.push(payload);
    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };
    const downQuery = {
      type: 'bulk',
      args: [],
    };
    const requestMsg = 'Deleting trigger...';
    const successMsg = 'Trigger deleted';
    const errorMsg = 'Delete trigger failed';

    const customOnSuccess = () => {
      // dispatch({ type: REQUEST_SUCCESS });
      dispatch(loadTriggers()).then(() => dispatch(push('/events/manage')));
      return;
    };
    const customOnError = () => {
      dispatch({ type: REQUEST_ERROR, data: errorMsg });
      return;
    };

    makeMigrationCall(
      dispatch,
      getState,
      upQuery.args,
      downQuery.args,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

/* ******************************************************* */
const eventReducer = (state = defaultState, action) => {
  // eslint-disable-line no-unused-vars
  if (action.type.indexOf('ProcessedEvents/') === 0) {
    return {
      ...state,
      view: processedEventsReducer(
        state.currentTrigger,
        state.triggerList,
        state.view,
        action
      ),
    };
  }
  if (action.type.indexOf('PendingEvents/') === 0) {
    return {
      ...state,
      view: pendingEventsReducer(
        state.currentTrigger,
        state.triggerList,
        state.view,
        action
      ),
    };
  }
  switch (action.type) {
    case LOAD_TRIGGER_LIST:
      return {
        ...state,
        triggerList: action.triggerList,
        listingTrigger: action.triggerList,
      };
    case LISTING_TRIGGER:
      return {
        ...state,
        listingTrigger: action.updatedList,
      };
    case LOAD_PROCESSED_EVENTS:
      return {
        ...state,
        processedEvents: action.data,
      };
    case LOAD_PENDING_EVENTS:
      return {
        ...state,
        pendingEvents: action.data,
      };
    case SET_TRIGGER:
      return { ...state, currentTrigger: action.triggerName };
    case ACCESS_KEY_ERROR:
      return { ...state, accessKeyError: action.data };
    case UPDATE_DATA_HEADERS:
      return { ...state, dataHeaders: action.data };
    default:
      return state;
  }
};

export default eventReducer;
export {
  setTrigger,
  loadTriggers,
  deleteTrigger,
  loadProcessedEvents,
  loadPendingEvents,
  handleMigrationErrors,
  makeMigrationCall,
  ACCESS_KEY_ERROR,
  UPDATE_DATA_HEADERS,
  LISTING_TRIGGER,
};
