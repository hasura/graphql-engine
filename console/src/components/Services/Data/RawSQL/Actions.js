import defaultState from './State';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import { loadSchema } from '../DataActions';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../Notification';
import {
  loadMigrationStatus,
  UPDATE_MIGRATION_STATUS_ERROR,
} from '../../../Main/Actions';
import dataHeaders from '../Common/Headers';
import returnMigrateUrl from '../Common/getMigrateUrl';

const MAKING_REQUEST = 'RawSQL/MAKING_REQUEST';
const SET_SQL = 'RawSQL/SET_SQL';
const SET_MIGRATION_CHECKED = 'RawSQL/SET_MIGRATION_CHECKED';
const SET_TRACK_TABLE_CHECKED = 'RawSQL/SET_TRACK_TABLE_CHECKED';
const REQUEST_SUCCESS = 'RawSQL/REQUEST_SUCCESS';
const REQUEST_ERROR = 'RawSQL/REQUEST_ERROR';

const MODAL_CLOSE = 'EditItem/MODAL_CLOSE';
const MODAL_OPEN = 'EditItem/MODAL_OPEN';

const modalOpen = () => ({ type: MODAL_OPEN });
const modalClose = () => ({ type: MODAL_CLOSE });

const executeSQL = isMigration => (dispatch, getState) => {
  dispatch({ type: MAKING_REQUEST });
  dispatch(showSuccessNotification('Executing the Query...'));

  const sql = getState().rawSQL.sql;
  const currMigrationMode = getState().main.migrationMode;

  const migrateUrl = returnMigrateUrl(currMigrationMode);
  const currentSchema = getState().tables.currentSchema;

  let url = Endpoints.rawSQL;
  let requestBody = {
    type: 'run_sql',
    args: { sql },
  };
  // check if its a migration and send to hasuractl migrate
  if (isMigration) {
    url = migrateUrl;
    const schemaChangesUp = [
      {
        type: 'run_sql',
        args: { sql },
      },
    ];
    // check if track view enabled
    if (getState().rawSQL.isTableTrackChecked) {
      const regExp = /create (view|table) (\S+)/;
      const matches = sql.match(regExp);
      let trackViewName = matches[2];
      if (trackViewName.indexOf('.') !== -1) {
        trackViewName = matches[2].split('.')[1];
      }
      const trackQuery = {
        type: 'add_existing_table_or_view',
        args: {
          name: trackViewName.trim(),
          schema: currentSchema,
        },
      };
      schemaChangesUp.push(trackQuery);
    }
    const upQuery = {
      type: 'bulk',
      args: schemaChangesUp,
    };
    const downQuery = {
      type: 'bulk',
      args: [],
    };
    const migrationName = 'run_sql_migration';
    requestBody = {
      name: migrationName,
      up: upQuery.args,
      down: downQuery.args,
    };
  }
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: dataHeaders(getState),
    body: JSON.stringify(requestBody),
  };
  fetch(url, options).then(
    response => {
      if (response.ok) {
        response.json().then(
          data => {
            if (isMigration) {
              dispatch(loadMigrationStatus());
            }
            dispatch(showSuccessNotification('SQL executed!'));
            dispatch(loadSchema()).then(() => {
              dispatch({ type: REQUEST_SUCCESS, data });
            });
          },
          err => {
            const parsedErrorMsg = err;
            parsedErrorMsg.message = JSON.parse(err.message);
            dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
            dispatch(
              showErrorNotification(
                'SQL execution failed!',
                'Something is wrong. Data sent back an invalid response json.',
                requestBody,
                parsedErrorMsg
              )
            );
            dispatch({
              type: REQUEST_ERROR,
              data:
                'Something is wrong. Data sent back an invalid response json.',
            });
            console.err('Error with response', err);
          }
        );
        return;
      }
      response.json().then(
        errorMsg => {
          dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: errorMsg });
          dispatch({ type: REQUEST_ERROR, data: errorMsg });
          const parsedErrorMsg = errorMsg;
          if (typeof parsedErrorMsg !== 'object') {
            parsedErrorMsg.message = JSON.parse(errorMsg).message;
          }
          if (parsedErrorMsg.code === 'data_api_error') {
            parsedErrorMsg.message = JSON.parse(errorMsg.message);
          } else if (parsedErrorMsg.code === 'postgres-error') {
            if (parsedErrorMsg.internal) {
              parsedErrorMsg.message = parsedErrorMsg.internal;
            } else {
              parsedErrorMsg.message = { error: parsedErrorMsg.error };
            }
          }
          dispatch(
            showErrorNotification(
              'SQL execution failed!',
              parsedErrorMsg.message.error,
              requestBody,
              parsedErrorMsg
            )
          );
        },
        () => {
          dispatch(
            showErrorNotification(
              'SQL execution failed!',
              'Something is wrong. Please check your configuration again'
            )
          );
          dispatch({
            type: REQUEST_ERROR,
            data: 'Something is wrong. Please check your configuration again',
          });
        }
      );
    },
    error => {
      console.error(error);
      dispatch(
        showErrorNotification(
          'SQL execution failed',
          'Cannot connect to server'
        )
      );
      dispatch({ type: REQUEST_ERROR, data: 'server-connection-failed' });
    }
  );
};

const rawSQLReducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_SQL:
      return { ...state, sql: action.data };
    case SET_MIGRATION_CHECKED:
      return { ...state, isMigrationChecked: action.data };
    case SET_TRACK_TABLE_CHECKED:
      return {
        ...state,
        isTableTrackChecked: action.data,
        showTrackTable: action.data,
      };
    case MAKING_REQUEST:
      return {
        ...state,
        ongoingRequest: true,
        lastError: null,
        lastSuccess: null,
      };
    case REQUEST_SUCCESS:
      if (action.data && action.data.result_type === 'CommandOk') {
        return {
          ...state,
          ongoingRequest: false,
          lastError: null,
          lastSuccess: true,
          resultType: 'command',
          result: [],
        };
      }
      return {
        ...state,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: true,
        resultType: 'tuples',
        result: action.data.result.slice(1),
        resultHeaders: action.data.result[0],
      };
    case REQUEST_ERROR:
      return {
        ...state,
        ongoingRequest: false,
        lastError: action.data,
        lastSuccess: null,
      };
    case MODAL_OPEN:
      return { ...state, isModalOpen: true };
    case MODAL_CLOSE:
      return { ...state, isModalOpen: false };
    default:
      return state;
  }
};

export default rawSQLReducer;
export {
  executeSQL,
  SET_SQL,
  SET_MIGRATION_CHECKED,
  SET_TRACK_TABLE_CHECKED,
  modalOpen,
  modalClose,
};
