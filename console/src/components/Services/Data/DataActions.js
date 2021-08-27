import React from 'react';
import sanitize from 'sanitize-filename';

import { getSchemaBaseRoute } from '../../Common/utils/routesUtils';
import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import defaultState from './DataState';
import viewReducer from './TableBrowseRows/ViewActions';
import editReducer from './TableBrowseRows/EditActions';
import modifyReducer from './TableCommon/TableReducer';
import { getAllUnTrackedRelations } from './TableRelationships/Actions';
import {
  showErrorNotification,
  showSuccessNotification,
  getErrorMessage,
  showNotification,
} from '../Common/Notification';
import dataHeaders from './Common/Headers';
import { loadMigrationStatus } from '../../Main/Actions';
import returnMigrateUrl from './Common/getMigrateUrl';
import { filterInconsistentMetadataObjects } from '../Settings/utils';
import globals from '../../../Globals';
import {
  fetchTrackedTableFkQuery,
  fetchTableListQuery,
  cascadeUpQueries,
  getDependencyError,
} from './utils';
import {
  mergeDataMssql,
  mergeLoadSchemaDataPostgres,
  mergeDataBigQuery,
  mergeDataCitus,
} from './mergeData';
import _push from './push';
import { convertArrayToJson } from './TableModify/utils';
import { CLI_CONSOLE_MODE, SERVER_CONSOLE_MODE } from '../../../constants';
import { maxAllowedMigrationLength } from './constants';
import { getDownQueryComments } from '../../../utils/migration/utils';
import { isEmpty } from '../../Common/utils/jsUtils';
import { currentDriver, dataSource } from '../../../dataSources';
import { exportMetadata } from '../../../metadata/actions';
import {
  getTablesFromAllSources,
  getTablesInfoSelector,
} from '../../../metadata/selector';
import { getRunSqlQuery } from '../../Common/utils/v1QueryUtils';
import { services } from '../../../dataSources/services';
import insertReducer from './TableInsertItem/InsertActions';
import {
  checkFeatureSupport,
  READ_ONLY_RUN_SQL_QUERIES,
} from '../../../helpers/versionUtils';

const SET_TABLE = 'Data/SET_TABLE';
const LOAD_FUNCTIONS = 'Data/LOAD_FUNCTIONS';
const LOAD_NON_TRACKABLE_FUNCTIONS = 'Data/LOAD_NON_TRACKABLE_FUNCTIONS';
const LOAD_SCHEMA = 'Data/LOAD_SCHEMA';
const LOAD_UNTRACKED_RELATIONS = 'Data/LOAD_UNTRACKED_RELATIONS';
const FETCH_SCHEMA_LIST = 'Data/FETCH_SCHEMA_LIST';
const UPDATE_CURRENT_SCHEMA = 'Data/UPDATE_CURRENT_SCHEMA';
const UPDATE_CURRENT_DATA_SOURCE = 'Data/UPDATE_CURRENT_DATA_SOURCE';
const ADMIN_SECRET_ERROR = 'Data/ADMIN_SECRET_ERROR';
const UPDATE_REMOTE_SCHEMA_MANUAL_REL = 'Data/UPDATE_SCHEMA_MANUAL_REL';
const SET_CONSISTENT_SCHEMA = 'Data/SET_CONSISTENT_SCHEMA';

const UPDATE_DATA_HEADERS = 'Data/UPDATE_DATA_HEADERS';

const FETCH_COLUMN_TYPE_INFO = 'Data/FETCH_COLUMN_TYPE_INFO';
const FETCH_COLUMN_TYPE_INFO_FAIL = 'Data/FETCH_COLUMN_TYPE_INFO_FAIL';
const RESET_COLUMN_TYPE_INFO = 'Data/RESET_COLUMN_TYPE_INFO';

const MAKE_REQUEST = 'ModifyTable/MAKE_REQUEST';
const REQUEST_SUCCESS = 'ModifyTable/REQUEST_SUCCESS';
const REQUEST_ERROR = 'ModifyTable/REQUEST_ERROR';

const SET_FILTER_SCHEMA = 'Data/SET_FILTER_SCHEMA';
const SET_FILTER_TABLES = 'Data/SET_FILTER_TABLES';

const SET_ADDITIONAL_COLUMNS_INFO = 'Data/SET_ADDITIONAL_COLUMNS_INFO';

export const SET_ALL_ROLES = 'Data/SET_ALL_ROLES';
export const setAllRoles = roles => ({
  type: SET_ALL_ROLES,
  roles,
});

const SET_DB_CONNECTION_ENV_VAR = 'Data/SET_DB_CONNECTION_ENV_VAR';
const RESET_DB_CONNECTION_ENV_VAR = 'Data/RESET_DB_CONNECTION_ENV_VAR';

const SET_ALL_SOURCES_SCHEMAS = 'Data/SET_ALL_SOURCES_SCHEMAS';
const SET_INCONSISTENT_INHERITED_ROLE = 'Data/SET_INCONSISTENT_INHERITED_ROLE';

export const mergeRemoteRelationshipsWithSchema = (
  remoteRelationships,
  table
) => {
  return (dispatch, getState) => {
    const { allSchemas } = getState().tables;
    const t = allSchemas.find(s => {
      return s.table_name === table.name && s.table_schema === table.schema;
    });
    if (!t) return;
    const newAllSchemas = allSchemas.filter(
      s => !(s.table_name === table.name && s.table_schema === table.schema)
    );
    newAllSchemas.push({
      ...t,
      remote_relationships: remoteRelationships,
    });
    dispatch({
      type: LOAD_SCHEMA,
      allSchemas: newAllSchemas,
    });
  };
};

export const setDBConnectionDetails = details => {
  return {
    type: SET_DB_CONNECTION_ENV_VAR,
    data: details,
  };
};

export const resetDBConnectionEnvVar = () => {
  return {
    type: RESET_DB_CONNECTION_ENV_VAR,
  };
};

const setUntrackedRelations = () => (dispatch, getState) => {
  const untrackedRelations = getAllUnTrackedRelations(
    getState().tables.allSchemas,
    getState().tables.currentSchema,
    getState().tables.currentDataSource
  ).bulkRelTrack;

  dispatch({
    type: LOAD_UNTRACKED_RELATIONS,
    untrackedRelations,
  });
};

// todo: it's called 4 times on start
const loadSchema = configOptions => {
  return (dispatch, getState) => {
    const url = Endpoints.query;

    let allSchemas = getState().tables.allSchemas;
    const source = getState().tables.currentDataSource;

    if (
      !configOptions ||
      ((!configOptions.schemas || configOptions.schemas.length === 0) &&
        (!configOptions.tables || configOptions.tables.length === 0))
    ) {
      configOptions = {
        schemas: [
          getState().tables.currentSchema || getState().tables.schemaList[0],
        ],
      };
    }

    if (configOptions) {
      if (configOptions.schemas) {
        allSchemas = allSchemas.filter(
          schemaInfo =>
            !configOptions.schemas.some(
              item => item === schemaInfo.table_schema
            )
        );
      }

      if (configOptions.tables) {
        allSchemas = allSchemas.filter(
          schemaInfo =>
            !configOptions.tables.some(
              item =>
                item.table_schema === schemaInfo.table_schema &&
                item.table_name === schemaInfo.table_name
            )
        );
      }
    }
    const body = {
      type: 'bulk',
      source,
      args: [
        fetchTableListQuery(configOptions, source),
        fetchTrackedTableFkQuery(configOptions, source),
        // todo: queries below could be done only when user visits `Data` page
        getRunSqlQuery(
          dataSource?.primaryKeysInfoSql(configOptions) || '',
          source,
          false,
          true
        ),
        getRunSqlQuery(
          dataSource?.uniqueKeysSql(configOptions) || '',
          source,
          false,
          true
        ),
      ],
    };

    if (dataSource?.checkConstraintsSql) {
      body.args.push(
        getRunSqlQuery(
          dataSource?.checkConstraintsSql(configOptions) || '',
          source,
          false,
          true
        )
      );
    }

    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(body),
    };

    return dispatch(exportMetadata()).then(state => {
      const metadataTables = getTablesInfoSelector(state)({});
      return dispatch(requestAction(url, options)).then(
        data => {
          if (!data || !data[0] || !data[0].result) return;

          let mergedData = [];
          switch (currentDriver) {
            case 'postgres':
              mergedData = mergeLoadSchemaDataPostgres(data, metadataTables);
              break;
            case 'citus':
              mergedData = mergeDataCitus(data, metadataTables);
              break;
            case 'mssql':
              mergedData = mergeDataMssql(data, metadataTables);
              break;
            case 'bigquery':
              mergedData = mergeDataBigQuery(data, metadataTables);
              break;
            default:
          }

          const { inconsistentObjects } = state.metadata;
          const maybeInconsistentSchemas = allSchemas.concat(mergedData);

          let consistentSchemas;
          if (inconsistentObjects.length > 0) {
            consistentSchemas = filterInconsistentMetadataObjects(
              maybeInconsistentSchemas,
              inconsistentObjects,
              'tables'
            );
          }

          dispatch({
            type: LOAD_SCHEMA,
            allSchemas: consistentSchemas || maybeInconsistentSchemas,
          });
        },
        error => {
          console.error('loadSchema error: ' + JSON.stringify(error));
          dispatch(
            showErrorNotification('DB schema loading failed', null, error)
          );
        }
      );
    });
  };
};

const fetchAdditionalColumnsInfo = () => (dispatch, getState) => {
  const schemaName = getState().tables.currentSchema;
  const currentSource = getState().tables.currentDataSource;

  if (!dataSource.getAdditionalColumnsInfoQuerySql) {
    // unavailable for a data source
    return;
  }
  const sql = dataSource.getAdditionalColumnsInfoQuerySql(schemaName);
  const query = getRunSqlQuery(sql, currentSource, false, true);

  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(Endpoints.query, options)).then(
    data => {
      if (data.result) {
        dispatch({
          type: SET_ADDITIONAL_COLUMNS_INFO,
          data: dataSource?.parseColumnsInfoResult(data.result),
        });
      }
    },
    error => {
      console.error(
        'Failed to load additional columns information ' + JSON.stringify(error)
      );
    }
  );
};

const updateSchemaInfo = options => (dispatch, getState) => {
  if (!getState().tables.currentDataSource) return;
  return dispatch(loadSchema(options)).then(data => {
    dispatch(fetchAdditionalColumnsInfo());
    dispatch(setUntrackedRelations());
    return data;
  });
};

const setConsistentSchema = data => ({
  type: SET_CONSISTENT_SCHEMA,
  data,
});

const updateSchemaList = (dispatch, getState, schemaList) => {
  dispatch({
    type: FETCH_SCHEMA_LIST,
    schemaList,
  });
  let newSchema = '';
  const { locationBeforeTransitions } = getState().routing;
  if (schemaList.length) {
    newSchema =
      dataSource.defaultRedirectSchema &&
      schemaList.includes(dataSource.defaultRedirectSchema)
        ? dataSource.defaultRedirectSchema
        : schemaList.sort(Intl.Collator().compare)[0];
  }
  if (
    locationBeforeTransitions &&
    !locationBeforeTransitions.pathname.includes('tables')
  )
    dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: newSchema });
  return dispatch(updateSchemaInfo());
};

const fetchDataInit = (source, driver) => (dispatch, getState) => {
  const url = Endpoints.query;

  const { schemaFilter } = getState().tables;

  if (driver === 'bigquery') {
    const schemaList = getState().metadata.metadataObject.sources.find(
      x => x.name === source
    ).configuration.datasets;
    return updateSchemaList(dispatch, getState, schemaList);
  }

  const currentSource = source || getState().tables.currentDataSource;
  const query = getRunSqlQuery(
    dataSource.schemaListSql(schemaFilter),
    currentSource,
    false,
    true,
    driver
  );

  if (!getState().tables.currentDataSource) return;

  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(url, options)).then(
    data => {
      const schemaList = data.result.reduce((acc, schema) => {
        if (schema[0] === 'schema_name') {
          return acc;
        }
        return [schema[0], ...acc];
      }, []);

      return updateSchemaList(dispatch, getState, schemaList);
    },
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
      return error;
    }
  );
};

const fetchFunctionInit = (schema = null) => (dispatch, getState) => {
  const url = Endpoints.query;
  const source = getState().tables.currentDataSource;

  const { schemaFilter } = getState().tables;
  let fnSchema = schema || getState().tables.currentSchema;
  if (schemaFilter && schemaFilter.length) {
    fnSchema = schemaFilter[0]; // todo: fix me
  }

  if (!source || !dataSource.getFunctionDefinitionSql) return;
  const body = {
    type: 'bulk',
    source,
    args: [
      getRunSqlQuery(
        dataSource.getFunctionDefinitionSql(fnSchema, null, 'trackable'),
        source,
        false,
        true
      ),
      getRunSqlQuery(
        dataSource.getFunctionDefinitionSql(fnSchema, null, 'non-trackable'),
        source,
        false,
        true
      ),
    ],
  };

  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(body),
  };

  return dispatch(requestAction(url, options)).then(
    data => {
      let trackable = [];
      let nonTrackable = [];
      try {
        trackable = JSON.parse(data[0].result[1]);
        nonTrackable = JSON.parse(data[1].result[1]);
      } catch (err) {
        console.error('Failed to fetch schema ' + JSON.stringify(err));
      }
      dispatch({ type: LOAD_FUNCTIONS, data: trackable });
      dispatch({ type: LOAD_NON_TRACKABLE_FUNCTIONS, data: nonTrackable });
    },
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
    }
  );
};

const updateCurrentSchema = (
  schemaName,
  sourceName,
  redirect = true,
  schemaList = []
) => dispatch => {
  if (schemaList.length && !schemaList.find(s => s === schemaName)) {
    schemaName = schemaList[0];
  }

  if (redirect) {
    dispatch(_push(getSchemaBaseRoute(schemaName, sourceName)));
  }

  return Promise.all([
    dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: schemaName }),
    dispatch(setUntrackedRelations()),
    dispatch(fetchFunctionInit()),
    dispatch(updateSchemaInfo()),
  ]);
};

/* ************ action creators *********************** */
const fetchSchemaList = () => (dispatch, getState) => {
  const url = Endpoints.query;
  const currentSource = getState().tables.currentDataSource;
  const { schemaFilter } = getState().tables;
  const query = getRunSqlQuery(
    dataSource.schemaListSql(schemaFilter),
    currentSource,
    false,
    true
  );

  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };
  return dispatch(requestAction(url, options)).then(
    data => {
      const schemaList = data.result.reduce((acc, schema) => {
        if (schema[0] === 'schema_name') {
          return acc;
        }
        return [schema[0], ...acc];
      }, []);
      dispatch({
        type: FETCH_SCHEMA_LIST,
        schemaList,
      });
      return data;
    },
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
      return error;
    }
  );
};

export const getSchemaList = (sourceType, sourceName) => (
  dispatch,
  getState
) => {
  const url = Endpoints.query;
  const sql = services[sourceType].schemaListSql();
  const query = getRunSqlQuery(sql, sourceName, false, true);
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };
  return dispatch(requestAction(url, options)).then(
    data => data,
    error => {
      console.error('Failed to fetch schema ' + JSON.stringify(error));
      return error;
    }
  );
};

export const getDatabaseSchemasInfo = (sourceType = 'postgres', sourceName) => (
  dispatch,
  getState
) => {
  const url = Endpoints.query;
  const sql = services[sourceType].getDatabaseInfo;
  const query = getRunSqlQuery(sql, sourceName, false, true);
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };
  return dispatch(requestAction(url, options)).then(
    ({ result }) => {
      if (!result.length > 1) {
        return {};
      }

      const trackedTables = getTablesFromAllSources(getState()).filter(
        ({ source }) => source === sourceName
      );
      const schemasInfo = {};

      JSON.parse(result[1]).forEach(i => {
        if (
          !trackedTables.some(
            t =>
              t.table.name === i.table_name && t.table.schema === i.table_schema
          )
        ) {
          return;
        }
        schemasInfo[i.table_schema] = {
          ...schemasInfo[i.table_schema],
          [i.table_name]: i.columns,
        };
      });

      return schemasInfo;
    },
    error => {
      console.error('Failed to fetch schemas info ' + JSON.stringify(error));
      return error;
    }
  );
};

export const getDatabaseTableTypeInfoForAllSources = schemaRequests => (
  dispatch,
  getState
) => {
  if (!schemaRequests.length) {
    return new Promise(resolve => {
      resolve([]);
    });
  }
  const url = Endpoints.query;

  const bulkQueries = [];
  schemaRequests.forEach(({ sourceType = 'postgres', sourceName, tables }) => {
    if (!tables.length) return;
    const sql = services[sourceType].getTableInfo(tables);
    bulkQueries.push(getRunSqlQuery(sql, sourceName, false, true, sourceType));
  });
  const query = { type: 'bulk', version: 1, args: bulkQueries };
  const options = {
    credentials: globalCookiePolicy,
    method: 'POST',
    headers: dataHeaders(getState),
    body: JSON.stringify(query),
  };
  return dispatch(requestAction(url, options)).then(
    results => {
      const schemas = results.map(({ result }, index) => {
        if (!result.length > 1) {
          return {};
        }
        const trackedTables = getTablesFromAllSources(getState()).filter(
          ({ source }) => source === bulkQueries[index]?.args?.source
        );
        const schemasInfo = {};

        let res;
        try {
          if (currentDriver === 'mssql') {
            res = JSON.parse(result.slice(1).join(''));
          } else if (currentDriver === 'bigquery') {
            res = result.slice(1).map(t => ({
              table_name: t[0],
              table_schema: t[1],
              table_type: t[2],
            }));
          } else {
            res = JSON.parse(result[1]);
          }
        } catch {
          res = [];
        }

        res.forEach(i => {
          if (
            !trackedTables.some(
              t =>
                t.table.name === i.table_name &&
                t.table.schema === i.table_schema
            )
          ) {
            return;
          }
          schemasInfo[i.table_schema] = {
            ...schemasInfo[i.table_schema],
            [i.table_name]: { table_type: i.table_type },
          };
        });
        return {
          source: bulkQueries[index]?.args?.source,
          schemaInfo: schemasInfo,
        };
      });
      let allSourceSchemas = {};
      if (schemas?.length > 0) {
        schemas.forEach(item => {
          allSourceSchemas[item.source] = item.schemaInfo;
        });
        const {
          tables: { allSourcesSchemas: existingSourcesSchemas = {} } = {},
        } = getState();
        allSourceSchemas = {
          ...existingSourcesSchemas,
          ...allSourceSchemas,
        };
        dispatch({
          type: SET_ALL_SOURCES_SCHEMAS,
          data: allSourceSchemas,
        });
      }
      return allSourceSchemas;
    },
    error => {
      console.error('Failed to fetch schemas info ' + JSON.stringify(error));
      return error;
    }
  );
};

const setTable = tableName => ({ type: SET_TABLE, tableName });

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

export const handleOutOfDateMetadata = dispatch => {
  return dispatch(
    showNotification(
      {
        title: 'Metadata is Out-of-Date',
        level: 'error',
        message: (
          <p>
            The operation failed as the metadata on the server is newer than
            what is currently loaded on the console. The metadata has to be
            re-fetched to continue editing it.
            <br />
            <br />
            Do you want fetch the latest metadata?
          </p>
        ),
        autoDismiss: 0,
        action: {
          label: (
            <>
              <i className="fa fa-refresh" aria-hidden="true" /> Fetch metadata
            </>
          ),
          callback: () => {
            dispatch(exportMetadata());
          },
        },
      },
      'error'
    )
  );
};

const makeMigrationCall = (
  dispatch,
  getState,
  upQueries,
  downQueries = [],
  migrationName,
  customOnSuccess,
  customOnError,
  requestMsg,
  successMsg,
  errorMsg,
  shouldSkipSchemaReload,
  skipExecution = false,
  isRetry = false
) => {
  const source = getState().tables.currentDataSource;
  const { resourceVersion } = getState().metadata;
  const upQuery = {
    type: 'bulk',
    source,
    resource_version: resourceVersion,
    args: upQueries,
  };

  const isRunSqlType =
    Array.isArray(upQueries) &&
    upQueries.length >= 0 &&
    upQueries.some(query => query?.type?.includes('run_sql') ?? false);

  if (downQueries && downQueries.length === 0 && isRunSqlType) {
    downQueries = getDownQueryComments(upQueries);
  }

  const trimmedMigrationName = migrationName.substring(
    0,
    maxAllowedMigrationLength
  );

  const migrationBody = {
    name: sanitize(trimmedMigrationName),
    up: upQuery.args,
    down: downQueries || [],
    datasource: source,
    skip_execution: skipExecution,
  };

  const currMigrationMode = getState().main.migrationMode;

  const migrateUrl = returnMigrateUrl(currMigrationMode, upQueries);

  let finalReqBody;
  if (globals.consoleMode === SERVER_CONSOLE_MODE) {
    finalReqBody = upQuery;
  } else if (globals.consoleMode === CLI_CONSOLE_MODE) {
    finalReqBody = migrationBody;
  }
  const url = migrateUrl;
  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: dataHeaders(getState),
    body: JSON.stringify(finalReqBody),
  };

  const onSuccess = data => {
    if (!shouldSkipSchemaReload) {
      if (globals.consoleMode === CLI_CONSOLE_MODE) {
        dispatch(loadMigrationStatus());
      }
      dispatch(updateSchemaInfo());
    }
    if (successMsg) {
      dispatch(showSuccessNotification(successMsg));
    }
    customOnSuccess(data, globals.consoleMode, currMigrationMode);
  };
  const retryMigration = (err = {}, errMsg = '', isPgCascade = false) => {
    const errorDetails = getErrorMessage('', err);
    const errorDetailsLines = errorDetails.split('\n');

    dispatch(
      showNotification(
        {
          title: errMsg,
          level: 'error',
          message: (
            <p>
              {errorDetailsLines.map((m, i) => (
                <div key={i}>{m}</div>
              ))}
              <br />
              Do you want to drop the dependent items as well?
            </p>
          ),
          autoDismiss: 0,
          action: {
            label: 'Continue',
            callback: () =>
              makeMigrationCall(
                dispatch,
                getState,
                cascadeUpQueries(upQueries, isPgCascade), // cascaded new up queries
                downQueries,
                migrationName,
                customOnSuccess,
                customOnError,
                requestMsg,
                successMsg,
                errorMsg,
                shouldSkipSchemaReload,
                false,
                true // prevent further retry
              ),
          },
        },
        'error'
      )
    );
  };

  const onError = err => {
    if (!isRetry) {
      const { dependencyError, sqlDependencyError } = getDependencyError(err);
      if (dependencyError) return retryMigration(dependencyError, errorMsg);
      if (sqlDependencyError)
        return retryMigration(sqlDependencyError, errorMsg, true);
    }

    if (err?.code === 'conflict') {
      dispatch(handleOutOfDateMetadata);
    } else {
      dispatch(handleMigrationErrors(errorMsg, err));
    }

    customOnError(err);
  };

  dispatch({ type: MAKE_REQUEST });
  dispatch(showSuccessNotification(requestMsg));
  return dispatch(
    requestAction(url, options, REQUEST_SUCCESS, REQUEST_ERROR)
  ).then(onSuccess, onError);
};

const getBulkColumnInfoFetchQuery = (schema, source) => {
  const fetchColumnTypes = getRunSqlQuery(
    dataSource.fetchColumnTypesQuery,
    source,
    false,
    true
  );
  const fetchTypeDefaultValues = getRunSqlQuery(
    dataSource.fetchColumnDefaultFunctions(schema),
    source,
    false,
    true
  );
  const fetchValidTypeCasts = getRunSqlQuery(
    dataSource.fetchColumnCastsQuery,
    source,
    false,
    true
  );

  return {
    type: 'bulk',
    source,
    args: [fetchColumnTypes, fetchTypeDefaultValues, fetchValidTypeCasts],
  };
};

const fetchColumnTypeInfo = () => {
  return (dispatch, getState) => {
    const url = Endpoints.query;
    const currState = getState();
    const { currentSchema, currentDataSource } = currState.tables;
    const reqQuery = getBulkColumnInfoFetchQuery(
      currentSchema,
      currentDataSource
    );
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(reqQuery),
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        if (currentDriver === 'mssql') {
          return dispatch({
            type: FETCH_COLUMN_TYPE_INFO,
            data: {
              columnDataTypes: [],
              columnTypeDefaultValues: {},
              columnTypeCasts: {},
            },
          });
        }

        const resultData = data[1].result.slice(1);
        const typeFuncsMap = {};

        resultData.forEach(r => {
          typeFuncsMap[r[1]] = r[0].split(',');
        });
        const columnDataTypeInfo = {
          columnDataTypes: data[0].result.slice(1),
          columnTypeDefaultValues: typeFuncsMap,
          columnTypeCasts: convertArrayToJson(data[2].result.slice(1)),
        };
        return dispatch({
          type: FETCH_COLUMN_TYPE_INFO,
          data: columnDataTypeInfo,
        });
      },
      error => {
        dispatch(
          showErrorNotification('Error fetching column types', null, error)
        );
        return dispatch({
          type: FETCH_COLUMN_TYPE_INFO_FAIL,
          data: error,
        });
      }
    );
  };
};

const fetchPartitionDetails = table => {
  return (dispatch, getState) => {
    const url = Endpoints.query;
    const currState = getState();
    const { currentDataSource } = currState.tables;
    const query = getRunSqlQuery(
      dataSource.getPartitionDetailsSql(table.table_name, table.table_schema),
      currentDataSource,
      false,
      true
    );
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(query),
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        try {
          const partitions = data.result.slice(1).map(row => ({
            parent_schema: row[0],
            parent_table: row[1],
            partition_name: row[2],
            partition_schema: row[3],
            partition_def: row[4],
            partition_key: row[5],
          }));
          return partitions;
        } catch (err) {
          console.log(err);
        }
      },
      error => {
        dispatch(
          showErrorNotification(
            'Error fetching partition information',
            null,
            error
          )
        );
      }
    );
  };
};

export const fetchTableIndexDetails = tableInfo => {
  return (dispatch, getState) => {
    const url = Endpoints.query;
    const currState = getState();
    const { currentDataSource } = currState.tables;
    const { table_name: table, table_schema: schema } = tableInfo;
    const query = getRunSqlQuery(
      dataSource.tableIndexSql({ table, schema }),
      currentDataSource,
      false,
      checkFeatureSupport(READ_ONLY_RUN_SQL_QUERIES) || false
    );
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(query),
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        try {
          return JSON.parse(data?.result?.[1]?.[0] ?? '[]');
        } catch (err) {
          console.error(err);
          return [];
        }
      },
      error => {
        dispatch(
          showErrorNotification(
            'Error fetching indexes information',
            null,
            error
          )
        );
      }
    );
  };
};

/* ******************************************************* */
const dataReducer = (state = defaultState, action) => {
  // eslint-disable-line no-unused-vars
  if (action.type.indexOf('ViewTable/') === 0) {
    return {
      ...state,
      view: viewReducer(
        state.currentTable,
        state.currentSchema,
        state.allSchemas,
        state.view,
        action
      ),
    };
  }
  if (action.type.indexOf('ModifyTable/') === 0) {
    return {
      ...state,
      modify: modifyReducer(
        state.currentTable,
        state.allSchemas,
        state.modify,
        action
      ),
    };
  }
  if (action.type.indexOf('InsertItem/') === 0) {
    return {
      ...state,
      insert: insertReducer(state.currentTable, state.insert, action),
    };
  }
  if (action.type.indexOf('EditItem/') === 0) {
    return {
      ...state,
      update: editReducer(state.currentTable, state.update, action),
    };
  }
  switch (action.type) {
    case LOAD_FUNCTIONS:
      return {
        ...state,
        postgresFunctions: action.data,
      };
    case LOAD_NON_TRACKABLE_FUNCTIONS:
      return {
        ...state,
        nonTrackablePostgresFunctions: action.data,
      };
    case LOAD_SCHEMA:
      // remove duplicates
      const result = action.allSchemas.reduce((unique, o) => {
        if (
          !unique.some(
            obj =>
              obj.table_name === o.table_name &&
              obj.table_schema === o.table_schema
          )
        ) {
          unique.push(o);
        }
        return unique;
      }, []);
      return {
        ...state,
        allSchemas: result,
      };
    case LOAD_UNTRACKED_RELATIONS:
      return {
        ...state,
        untrackedRelations: action.untrackedRelations,
      };
    case SET_TABLE:
      return { ...state, currentTable: action.tableName };
    case FETCH_SCHEMA_LIST:
      return { ...state, schemaList: action.schemaList };
    case SET_CONSISTENT_SCHEMA:
      return { ...state, allSchemas: action.data, listingSchemas: action.data };
    case UPDATE_CURRENT_SCHEMA:
      return { ...state, currentSchema: action.currentSchema };
    case UPDATE_CURRENT_DATA_SOURCE:
      return { ...state, currentDataSource: action.source };
    case ADMIN_SECRET_ERROR:
      return { ...state, adminSecretError: action.data };
    case UPDATE_DATA_HEADERS:
      return { ...state, dataHeaders: action.data };
    case UPDATE_REMOTE_SCHEMA_MANUAL_REL:
      return {
        ...state,
        modify: {
          ...state.modify,
          manualRelAdd: {
            ...state.modify.manualRelAdd,
            rSchema: action.data,
          },
        },
      };
    case FETCH_COLUMN_TYPE_INFO:
      return {
        ...state,
        columnDataTypes: action.data.columnDataTypes,
        columnDefaultFunctions: action.data.columnTypeDefaultValues,
        columnDataTypeInfoErr: null,
        columnTypeCasts: action.data.columnTypeCasts,
      };

    case FETCH_COLUMN_TYPE_INFO_FAIL:
      return {
        ...state,
        columnDataTypes: [],
        columnDefaultFunctions: {},
        columnTypeCasts: {},
        columnDataTypeInfoErr: action.data,
      };
    case RESET_COLUMN_TYPE_INFO:
      return {
        ...state,
        columnDataTypes: [...defaultState.columnDataTypes],
        columnDefaultFunctions: { ...defaultState.columnDefaultFunctions },
        columnTypeCasts: { ...defaultState.columnTypeCasts },
        columnDataTypeInfoErr: defaultState.columnDataTypeInfoErr,
      };
    case SET_ADDITIONAL_COLUMNS_INFO:
      if (isEmpty(action.data)) return state;
      return {
        ...state,
        allSchemas: state.allSchemas.map(schema => {
          if (!action.data[schema.table_name]) return schema;
          return {
            ...schema,
            columns: schema.columns.map(column => ({
              ...column,
              ...action.data[schema.table_name][column.column_name],
            })),
          };
        }),
      };
    case SET_FILTER_SCHEMA:
      return {
        ...state,
        schemaFilter: action.data,
      };
    case SET_FILTER_TABLES:
      return {
        ...state,
        tableFilter: action.data,
      };
    case SET_DB_CONNECTION_ENV_VAR:
      return {
        ...state,
        dbConnection: action.data,
      };
    case SET_ALL_SOURCES_SCHEMAS:
      return {
        ...state,
        allSourcesSchemas: action.data,
      };
    case RESET_DB_CONNECTION_ENV_VAR:
      return {
        ...state,
        dbConnection: {
          envVar: '',
          dbURL: '',
        },
      };
    case SET_INCONSISTENT_INHERITED_ROLE:
      return {
        ...state,
        modify: {
          ...state.modify,
          permissionsState: {
            ...state.modify.permissionsState,
            inconsistentInhertiedRole: action.inconsistent_inherited_role,
          },
        },
      };
    default:
      return state;
  }
};

export default dataReducer;
export {
  MAKE_REQUEST,
  REQUEST_SUCCESS,
  REQUEST_ERROR,
  setTable,
  updateSchemaInfo,
  handleMigrationErrors,
  makeMigrationCall,
  LOAD_UNTRACKED_RELATIONS,
  UPDATE_CURRENT_SCHEMA,
  UPDATE_CURRENT_DATA_SOURCE,
  fetchSchemaList,
  fetchDataInit,
  fetchFunctionInit,
  updateCurrentSchema,
  ADMIN_SECRET_ERROR,
  UPDATE_DATA_HEADERS,
  UPDATE_REMOTE_SCHEMA_MANUAL_REL,
  LOAD_SCHEMA,
  setConsistentSchema,
  fetchColumnTypeInfo,
  RESET_COLUMN_TYPE_INFO,
  setUntrackedRelations,
  SET_ADDITIONAL_COLUMNS_INFO,
  fetchAdditionalColumnsInfo,
  SET_FILTER_SCHEMA,
  SET_FILTER_TABLES,
  fetchPartitionDetails,
  SET_INCONSISTENT_INHERITED_ROLE,
};
