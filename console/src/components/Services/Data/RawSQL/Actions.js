import defaultState from './State';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import {
  handleMigrationErrors,
  fetchDataInit,
  handleOutOfDateMetadata,
} from '../DataActions';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../../Common/Notification';
import {
  loadMigrationStatus,
  UPDATE_MIGRATION_STATUS_ERROR,
} from '../../../Main/Actions';
import { parseCreateSQL } from './utils';
import dataHeaders from '../Common/Headers';
import returnMigrateUrl from '../Common/getMigrateUrl';
import requestAction from '../../../../utils/requestAction';
import {
  dataSource,
  escapeTableColumns,
  escapeTableName,
  findTable,
} from '../../../../dataSources';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';
import { getDownQueryComments } from '../../../../utils/migration/utils';
import {
  getTrackFunctionQuery,
  getTrackTableQuery,
} from '../../../../metadata/queryUtils';
import globals from '../../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import { exportMetadata } from '../../../../metadata/actions';

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

const trackAllItems = (sql, isMigration, migrationName, source, driver) => (
  dispatch,
  getState
) => {
  const currMigrationMode = getState().main.migrationMode;
  const { resourceVersion } = getState().metadata;

  const objects = parseCreateSQL(sql, driver);
  const changes = [];
  objects.forEach(({ type, name, schema, isPartition }) => {
    if (isPartition) return;
    let req = {};
    if (type === 'function') {
      req = getTrackFunctionQuery(name, schema, source, {}, driver);
    } else {
      const tableDef = { name, schema };
      const { allSchemas } = getState().tables;
      const table = findTable(allSchemas, tableDef);
      req = getTrackTableQuery({
        tableDef,
        source,
        driver,
        customColumnNames: escapeTableColumns(table),
        customName: escapeTableName(name),
      });
    }
    changes.push(req);
  });

  let url = Endpoints.metadata;
  let request;
  if (isMigration) {
    if (globals.consoleMode === CLI_CONSOLE_MODE) {
      url = currMigrationMode
        ? Endpoints.hasuractlMigrate
        : Endpoints.hasuractlMetadata;
    }
    request = {
      name: migrationName,
      datasource: source,
      up: changes,
      down: [],
    };
  } else {
    request = {
      type: 'bulk',
      source: source,
      args: changes,
      resource_version: resourceVersion,
    };
  }

  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: dataHeaders(getState),
    body: JSON.stringify(request),
  };

  return dispatch(requestAction(url, options)).then(
    () => {
      dispatch(showSuccessNotification('Items were tracked successfuly'));
    },
    err => {
      if (err.code === 'conflict') {
        dispatch(handleOutOfDateMetadata);
      }
      dispatch(showErrorNotification('Tracking items failed', err.code, err));
      throw new Error(err);
    }
  );
};

const executeSQL = (
  isMigration,
  migrationName,
  statementTimeout,
  source,
  driver
) => (dispatch, getState) => {
  dispatch({ type: MAKING_REQUEST });
  dispatch(showSuccessNotification('Executing the Query...'));

  const { isTableTrackChecked, isCascadeChecked, sql } = getState().rawSQL;
  const { migrationMode, readOnlyMode } = getState().main;

  const isStatementTimeout = statementTimeout && !isMigration;
  const migrateUrl = returnMigrateUrl(migrationMode);
  let url = Endpoints.query;
  const schemaChangesUp = [];

  if (isStatementTimeout && dataSource.getStatementTimeoutSql) {
    schemaChangesUp.push(
      getRunSqlQuery(
        dataSource.getStatementTimeoutSql(statementTimeout),
        source,
        false,
        readOnlyMode,
        driver
      )
    );
  }

  schemaChangesUp.push(
    getRunSqlQuery(sql, source, isCascadeChecked, readOnlyMode, driver)
  );

  const schemaChangesDown = getDownQueryComments(schemaChangesUp, source);

  let requestBody = {
    type: 'bulk',
    source,
    args: schemaChangesUp,
  };

  // check if its a migration and send to hasuractl migrate
  if (isMigration) {
    url = migrateUrl;
    requestBody = {
      name: migrationName,
      up: schemaChangesUp,
      down: schemaChangesDown,
      datasource: source,
    };
  }
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: dataHeaders(getState),
    body: JSON.stringify(requestBody),
  };

  const callback = data => {
    if (isMigration) {
      dispatch(loadMigrationStatus());
    }
    dispatch(showSuccessNotification('SQL executed!'));
    dispatch(fetchDataInit(source, driver)).then(() => {
      dispatch({
        type: REQUEST_SUCCESS,
        data: data && (isStatementTimeout ? data[1] : data[0]),
      });
    });
  };

  return dispatch(requestAction(url, options))
    .then(
      data => {
        if (isTableTrackChecked) {
          dispatch(exportMetadata()).then(() => {
            dispatch(
              trackAllItems(sql, isMigration, migrationName, source, driver)
            ).then(() => callback(data));
          });
          return;
        }
        callback(data);
      },
      err => {
        const title = 'SQL Execution Failed';
        dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
        dispatch({ type: REQUEST_ERROR, data: err });
        if (isMigration) {
          dispatch(handleMigrationErrors(title, err));
        } else {
          dispatch(showErrorNotification(title, err.code, err));
        }
      }
    )
    .catch(errorMsg => {
      const parsedErrorMsg = errorMsg;
      parsedErrorMsg.message = JSON.parse(errorMsg.message);
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: errorMsg });
      dispatch(
        showErrorNotification(
          'SQL execution failed!',
          'Something is wrong. Received an invalid response json.',
          parsedErrorMsg
        )
      );
      dispatch({
        type: REQUEST_ERROR,
        data: 'Something is wrong. Received an invalid response json.',
      });
      console.err('RunSQL error: ', errorMsg);
    });
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
        result: action?.data?.result?.slice?.(1) ?? [],
        resultHeaders: action?.data?.result?.[0] ?? [],
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
