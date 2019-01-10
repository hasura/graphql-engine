import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import defaultState from './EventState';
import processedEventsReducer from './ProcessedEvents/ViewActions';
import pendingEventsReducer from './PendingEvents/ViewActions';
import runningEventsReducer from './RunningEvents/ViewActions';
import streamingLogsReducer from './StreamingLogs/LogActions';
import { showErrorNotification, showSuccessNotification } from './Notification';
import dataHeaders from './Common/Headers';
import { loadMigrationStatus } from '../../Main/Actions';
import returnMigrateUrl from './Common/getMigrateUrl';
import globals from '../../../Globals';
import push from './push';
import { initQueries } from '../Data/DataActions';
import { replace } from 'react-router-redux';

import { SERVER_CONSOLE_MODE } from '../../../constants';
import { REQUEST_COMPLETE, REQUEST_ONGOING } from './Modify/Actions';

const SET_TRIGGER = 'Event/SET_TRIGGER';
const LOAD_TRIGGER_LIST = 'Event/LOAD_TRIGGER_LIST';
const LOAD_PROCESSED_EVENTS = 'Event/LOAD_PROCESSED_EVENTS';
const LOAD_PENDING_EVENTS = 'Event/LOAD_PENDING_EVENTS';
const LOAD_RUNNING_EVENTS = 'Event/LOAD_RUNNING_EVENTS';
const ACCESS_KEY_ERROR = 'Event/ACCESS_KEY_ERROR';
const UPDATE_DATA_HEADERS = 'Event/UPDATE_DATA_HEADERS';
const LISTING_TRIGGER = 'Event/LISTING_TRIGGER';
const LOAD_EVENT_LOGS = 'Event/LOAD_EVENT_LOGS';
const LOAD_EVENT_TABLE_SCHEMA = 'Event/LOAD_EVENT_TABLE_SCHEMA';
const MODAL_OPEN = 'Event/MODAL_OPEN';
const SET_REDELIVER_EVENT = 'Event/SET_REDELIVER_EVENT';
const LOAD_EVENT_INVOCATIONS = 'Event/LOAD_EVENT_INVOCATIONS';
const REDELIVER_EVENT_SUCCESS = 'Event/REDELIVER_EVENT_SUCCESS';
const REDELIVER_EVENT_FAILURE = 'Event/REDELIVER_EVENT_FAILURE';

const MAKE_REQUEST = 'Event/MAKE_REQUEST';
const REQUEST_SUCCESS = 'Event/REQUEST_SUCCESS';
const REQUEST_ERROR = 'Event/REQUEST_ERROR';

/* ************ action creators *********************** */
const loadTriggers = () => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const body = {
    type: 'bulk',
    args: [
      {
        type: 'select',
        args: {
          table: {
            name: 'event_triggers',
            schema: 'hdb_catalog',
          },
          columns: ['*'],
        },
      },
      initQueries.loadSchema,
    ],
  };
  body.args[1].args.where = {
    table_schema: {
      $nin: ['information_schema', 'pg_catalog', 'hdb_catalog', 'hdb_views'],
    },
  };
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_EVENT_TABLE_SCHEMA, data: data[1] });
      dispatch({ type: LOAD_TRIGGER_LIST, triggerList: data[0] });
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
            where: { delivered: false, error: false, tries: 0 },
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

const loadRunningEvents = () => (dispatch, getState) => {
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
            where: { delivered: false, error: false, tries: { $gt: 0 } },
            order_by: ['-created_at'],
            limit: 10,
          },
        ],
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_RUNNING_EVENTS, data: data });
    },
    error => {
      console.error('Failed to load triggers' + JSON.stringify(error));
    }
  );
};

const loadEventLogs = triggerName => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const triggerOptions = {
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
        columns: ['*'],
        where: {
          name: triggerName,
        },
      },
    }),
  };
  return dispatch(requestAction(url, triggerOptions)).then(
    triggerData => {
      if (triggerData.length !== 0) {
        const body = {
          type: 'bulk',
          args: [
            {
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
                where: { event: { trigger_id: triggerData[0].id } },
                order_by: ['-created_at'],
                limit: 10,
              },
            },
          ],
        };
        const logOptions = {
          credentials: globalCookiePolicy,
          method: 'POST',
          headers: dataHeaders(getState),
          body: JSON.stringify(body),
        };
        dispatch(requestAction(url, logOptions)).then(
          logsData => {
            dispatch({ type: LOAD_EVENT_LOGS, data: logsData[0] });
          },
          error => {
            console.error(
              'Failed to load trigger logs' + JSON.stringify(error)
            );
          }
        );
      } else {
        dispatch(replace('/404'));
      }
    },
    error => {
      console.error(
        'Failed to fetch trigger information' + JSON.stringify(error)
      );
    }
  );
};

const loadEventInvocations = eventId => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
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
        where: { event_id: eventId },
        order_by: ['-created_at'],
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: LOAD_EVENT_INVOCATIONS, data: data });
    },
    error => {
      console.error('Failed to load triggers' + JSON.stringify(error));
    }
  );
};

const redeliverEvent = eventId => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify({
      type: 'deliver_event',
      args: {
        event_id: eventId,
      },
    }),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      dispatch({ type: REDELIVER_EVENT_SUCCESS, data: data });
    },
    error => {
      console.error('Failed to load triggers' + JSON.stringify(error));
      dispatch({ type: REDELIVER_EVENT_FAILURE, data: error });
    }
  );
};

const setTrigger = triggerName => ({ type: SET_TRIGGER, triggerName });

const setRedeliverEvent = eventId => dispatch => {
  /*
    Redeliver event and mark the redeliverEventId to the redelivered event so that it can be tracked.
  */
  return dispatch(redeliverEvent(eventId)).then(() => {
    return Promise.all([
      dispatch({ type: SET_REDELIVER_EVENT, eventId }),
      dispatch(loadEventInvocations(eventId)),
    ]);
  });
};

/* **********Shared functions between table actions********* */

const handleMigrationErrors = (title, errorMsg) => dispatch => {
  const requestMsg = title;
  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
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
  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
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
      dispatch(loadMigrationStatus()); // don't call for server mode
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

    const triggerList = getState().triggers.triggerList;
    const currentTriggerInfo = triggerList.filter(
      t => t.name === triggerName
    )[0];
    // apply migrations
    const migrationName = 'delete_trigger_' + triggerName.trim();
    const payload = {
      type: 'delete_event_trigger',
      args: {
        name: triggerName,
      },
    };
    const downPayload = {
      type: 'create_event_trigger',
      args: {
        name: triggerName,
        table: {
          name: currentTriggerInfo.table_name,
          schema: currentTriggerInfo.schema_name,
        },
        webhook: currentTriggerInfo.webhook,
      },
    };
    const upQueryArgs = [];
    upQueryArgs.push(payload);
    const downQueryArgs = [];
    downQueryArgs.push(downPayload);
    const upQuery = {
      type: 'bulk',
      args: upQueryArgs,
    };
    const downQuery = {
      type: 'bulk',
      args: downQueryArgs,
    };
    const requestMsg = 'Deleting trigger...';
    const successMsg = 'Trigger deleted';
    const errorMsg = 'Delete trigger failed';

    const customOnSuccess = () => {
      // dispatch({ type: REQUEST_SUCCESS });
      dispatch({ type: REQUEST_COMPLETE }); // modify trigger action
      dispatch(loadTriggers()).then(() => dispatch(push('/manage/triggers')));
      return;
    };
    const customOnError = () => {
      dispatch({ type: REQUEST_COMPLETE }); // modify trigger action
      dispatch({ type: REQUEST_ERROR, data: errorMsg });
      return;
    };

    // modify trigger action
    dispatch({ type: REQUEST_ONGOING, data: 'delete' });

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
  if (action.type.indexOf('RunningEvents/') === 0) {
    return {
      ...state,
      view: runningEventsReducer(
        state.currentTrigger,
        state.triggerList,
        state.view,
        action
      ),
    };
  }
  if (action.type.indexOf('StreamingLogs/') === 0) {
    return {
      ...state,
      log: streamingLogsReducer(
        state.currentTrigger,
        state.triggerList,
        state.log,
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
    case LOAD_RUNNING_EVENTS:
      return {
        ...state,
        runningEvents: action.data,
      };
    case LOAD_EVENT_LOGS:
      return {
        ...state,
        log: { ...state.log, rows: action.data, count: action.data.length },
      };
    case LOAD_EVENT_TABLE_SCHEMA:
      return {
        ...state,
        tableSchemas: action.data,
      };
    case SET_TRIGGER:
      return { ...state, currentTrigger: action.triggerName };
    case ACCESS_KEY_ERROR:
      return { ...state, accessKeyError: action.data };
    case UPDATE_DATA_HEADERS:
      return { ...state, dataHeaders: action.data };
    case MODAL_OPEN:
      return {
        ...state,
        log: { ...state.log, isModalOpen: action.data },
      };
    case SET_REDELIVER_EVENT:
      return {
        ...state,
        log: { ...state.log, redeliverEventId: action.eventId },
      };
    case LOAD_EVENT_INVOCATIONS:
      return {
        ...state,
        log: { ...state.log, eventInvocations: action.data },
      };
    case REDELIVER_EVENT_SUCCESS:
      return {
        ...state,
        log: { ...state.log, redeliverInvocationId: action.data },
      };
    case REDELIVER_EVENT_FAILURE:
      return {
        ...state,
        log: { ...state.log, redeliverEventFailure: action.data },
      };
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
  loadRunningEvents,
  loadEventLogs,
  handleMigrationErrors,
  makeMigrationCall,
  setRedeliverEvent,
  loadEventInvocations,
  redeliverEvent,
  ACCESS_KEY_ERROR,
  UPDATE_DATA_HEADERS,
  LISTING_TRIGGER,
  MODAL_OPEN,
  SET_REDELIVER_EVENT,
};
