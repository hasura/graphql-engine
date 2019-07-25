import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import defaultState from './EventState';
import processedEventsReducer from './ProcessedEvents/ViewActions';
import pendingEventsReducer from './PendingEvents/ViewActions';
import runningEventsReducer from './RunningEvents/ViewActions';
import streamingLogsReducer from './StreamingLogs/LogActions';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../Common/Notification';
import dataHeaders from './Common/Headers';
import { loadMigrationStatus } from '../../Main/Actions';
import returnMigrateUrl from './Common/getMigrateUrl';
import globals from '../../../Globals';
import push from './push';
import { loadInconsistentObjects } from '../Metadata/Actions';
import { filterInconsistentMetadataObjects } from '../Metadata/utils';
import { replace } from 'react-router-redux';
import { getEventTriggersQuery } from './utils';

import { SERVER_CONSOLE_MODE } from '../../../constants';
import { REQUEST_COMPLETE, REQUEST_ONGOING } from './Modify/Actions';

const SET_TRIGGER = 'Event/SET_TRIGGER';
const LOAD_TRIGGER_LIST = 'Event/LOAD_TRIGGER_LIST';
const LOAD_PROCESSED_EVENTS = 'Event/LOAD_PROCESSED_EVENTS';
const LOAD_PENDING_EVENTS = 'Event/LOAD_PENDING_EVENTS';
const LOAD_RUNNING_EVENTS = 'Event/LOAD_RUNNING_EVENTS';
const ADMIN_SECRET_ERROR = 'Event/ADMIN_SECRET_ERROR';
const UPDATE_DATA_HEADERS = 'Event/UPDATE_DATA_HEADERS';
const LISTING_TRIGGER = 'Event/LISTING_TRIGGER';
const LOAD_EVENT_LOGS = 'Event/LOAD_EVENT_LOGS';
const MODAL_OPEN = 'Event/MODAL_OPEN';
const SET_REDELIVER_EVENT = 'Event/SET_REDELIVER_EVENT';
const LOAD_EVENT_INVOCATIONS = 'Event/LOAD_EVENT_INVOCATIONS';
const REDELIVER_EVENT_SUCCESS = 'Event/REDELIVER_EVENT_SUCCESS';
const REDELIVER_EVENT_FAILURE = 'Event/REDELIVER_EVENT_FAILURE';

const MAKE_REQUEST = 'Event/MAKE_REQUEST';
const REQUEST_SUCCESS = 'Event/REQUEST_SUCCESS';
const REQUEST_ERROR = 'Event/REQUEST_ERROR';

/* ************ action creators *********************** */
const loadTriggers = triggerNames => (dispatch, getState) => {
  const url = Endpoints.getSchema;
  const body = getEventTriggersQuery(triggerNames);
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      if (data.result_type !== 'TuplesOk') {
        console.error('Failed to event trigger info' + JSON.stringify(data[1]));
        return;
      }
      let triggerData = JSON.parse(data.result[1]);
      if (triggerNames.length !== 0) {
        // getExisting state
        const existingTriggers = getState().triggers.triggerList.filter(
          trigger => triggerNames.some(item => item !== trigger.name)
        );
        const triggerLists = existingTriggers.concat(triggerData);
        triggerData = triggerLists.sort((a, b) => {
          return a.name === b.name ? 0 : +(a.name > b.name) || -1;
        });
      }
      const { inconsistentObjects } = getState().metadata;
      let consistentTriggers;
      if (inconsistentObjects.length > 1) {
        consistentTriggers = filterInconsistentMetadataObjects(
          triggerData,
          inconsistentObjects,
          'events'
        );
      }
      dispatch({
        type: LOAD_TRIGGER_LIST,
        triggerList: consistentTriggers || triggerData,
      });
      dispatch(loadInconsistentObjects(false));
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
                where: { event: { trigger_name: triggerData[0].name } },
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
      type: 'redeliver_event',
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
  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
    // handle errors for run_sql based workflow
    dispatch(showErrorNotification(title, errorMsg.code, errorMsg));
  } else if (errorMsg.code === 'migration_failed') {
    dispatch(showErrorNotification(title, 'Migration Failed', errorMsg));
  } else if (errorMsg.code === 'data_api_error') {
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(
      showErrorNotification(title, parsedErrorMsg.message.error, parsedErrorMsg)
    );
  } else {
    // any other unhandled codes
    const parsedErrorMsg = errorMsg;
    parsedErrorMsg.message = JSON.parse(errorMsg.message);
    dispatch(showErrorNotification(title, errorMsg.code, parsedErrorMsg));
  }
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
    const currentTriggerInfo = getState().triggers.triggerList.filter(
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
          schema: currentTriggerInfo.table_schema,
        },
        retry_conf: { ...currentTriggerInfo.configuration.retry_conf },
        ...currentTriggerInfo.configuration.definition,
        headers: [...currentTriggerInfo.configuration.headers],
      },
    };
    if (currentTriggerInfo.configuration.webhook_from_env) {
      downPayload.args.webhook_from_env =
        currentTriggerInfo.configuration.webhook_from_env;
    } else {
      downPayload.args.webhook = currentTriggerInfo.configuration.webhook;
    }
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
      dispatch(showSuccessNotification('Trigger Deleted'));
      dispatch(push('/manage/triggers'));
      // remove this trigger from state
      const existingTriggers = getState().triggers.triggerList.filter(
        trigger => trigger.name !== triggerName
      );
      dispatch({
        type: LOAD_TRIGGER_LIST,
        triggerList: existingTriggers,
      });
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
      errorMsg,
      true
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
    case SET_TRIGGER:
      return { ...state, currentTrigger: action.triggerName };
    case ADMIN_SECRET_ERROR:
      return { ...state, adminSecretError: action.data };
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
  loadPendingEvents,
  loadRunningEvents,
  loadEventLogs,
  handleMigrationErrors,
  makeMigrationCall,
  setRedeliverEvent,
  loadEventInvocations,
  redeliverEvent,
  ADMIN_SECRET_ERROR,
  UPDATE_DATA_HEADERS,
  LISTING_TRIGGER,
  MODAL_OPEN,
  SET_REDELIVER_EVENT,
};
