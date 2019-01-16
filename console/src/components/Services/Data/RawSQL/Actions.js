import defaultState from './State';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import { handleMigrationErrors, fetchDataInit } from '../DataActions';
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
const SET_CASCADE_CHECKED = 'RawSQL/SET_CASCADE_CHECKED';
const SET_MIGRATION_CHECKED = 'RawSQL/SET_MIGRATION_CHECKED';
const SET_TRACK_TABLE_CHECKED = 'RawSQL/SET_TRACK_TABLE_CHECKED';
const REQUEST_SUCCESS = 'RawSQL/REQUEST_SUCCESS';
const REQUEST_ERROR = 'RawSQL/REQUEST_ERROR';

const MODAL_CLOSE = 'EditItem/MODAL_CLOSE';
const MODAL_OPEN = 'EditItem/MODAL_OPEN';

const modalOpen = () => ({ type: MODAL_OPEN });
const modalClose = () => ({ type: MODAL_CLOSE });

const executeSQL = (isMigration, migrationName) => (dispatch, getState) => {
  dispatch({ type: MAKING_REQUEST });
  dispatch(showSuccessNotification('Executing the Query...'));

  const sql = getState().rawSQL.sql;
  const currMigrationMode = getState().main.migrationMode;

  const migrateUrl = returnMigrateUrl(currMigrationMode);
  const currentSchema = 'public';
  const isCascadeChecked = getState().rawSQL.isCascadeChecked;

  let url = Endpoints.rawSQL;
  const schemaChangesUp = [
    {
      type: 'run_sql',
      args: { sql: sql, cascade: isCascadeChecked },
    },
  ];
  // check if track view enabled
  if (getState().rawSQL.isTableTrackChecked) {
    const regExp = /create\s*(?:|or\s*replace)\s*(view|table)\s*((\"?\w+\"?)\.(\"?\w+\"?)|(\"?\w+\"?))/; // eslint-disable-line
    const matches = sql.match(new RegExp(regExp, 'gmi'));
    if (matches) {
      matches.forEach(element => {
        const itemMatch = element.match(new RegExp(regExp, 'i'));
        if (itemMatch && itemMatch.length === 6) {
          const trackQuery = {
            type: 'add_existing_table_or_view',
            args: {},
          };
          // If group 5 is undefined, use group 3 and 4 for schema and table respectively
          // If group 5 is present, use group 5 for table name using public schema.
          if (itemMatch[5]) {
            trackQuery.args.name = itemMatch[5];
            trackQuery.args.schema = currentSchema;
          } else {
            trackQuery.args.name = itemMatch[4];
            trackQuery.args.schema = itemMatch[3];
          }
          // replace and trim schema and table name
          trackQuery.args.name = trackQuery.args.name
            .replace(/['"]+/g, '')
            .trim();
          trackQuery.args.schema = trackQuery.args.schema
            .replace(/['"]+/g, '')
            .trim();
          schemaChangesUp.push(trackQuery);
        }
      });
    }
  }
  let requestBody = {
    type: 'bulk',
    args: schemaChangesUp,
  };
  // check if its a migration and send to hasuractl migrate
  if (isMigration) {
    url = migrateUrl;
    requestBody = {
      name: migrationName,
      up: schemaChangesUp,
      down: [],
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
            dispatch(fetchDataInit()).then(() => {
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
          dispatch(handleMigrationErrors('SQL Execution Failed', errorMsg));
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
    case SET_CASCADE_CHECKED:
      return { ...state, isCascadeChecked: action.data };
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
      if (
        action.data &&
        action.data[0] &&
        action.data[0].result_type === 'CommandOk'
      ) {
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
        result: action.data[0].result.slice(1),
        resultHeaders: action.data[0].result[0],
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
  SET_CASCADE_CHECKED,
  SET_MIGRATION_CHECKED,
  SET_TRACK_TABLE_CHECKED,
  modalOpen,
  modalClose,
};
