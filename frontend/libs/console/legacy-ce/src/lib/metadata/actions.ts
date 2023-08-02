import { MetadataResponse } from '../features/MetadataAPI';
import { Api } from '../hooks/apiUtils';
import { getRunSqlQuery } from '../components/Common/utils/v1QueryUtils';
import dataHeaders from '../components/Services/Data/Common/Headers';
import {
  ConnectDBEvent,
  sendTelemetryEvent,
  trackRuntimeError,
} from '../telemetry';
import { RunSQLSelectResponse } from '../features/DataSource';
import requestAction from '../utils/requestAction';
import Endpoints, { globalCookiePolicy } from '../Endpoints';

import {
  ConnectionParams,
  ConnectionPoolSettings,
  HasuraMetadataV2,
  HasuraMetadataV3,
  InconsistentObject,
  IsolationLevelOptions,
  MetadataDataSource,
  RestEndpointEntry,
  SourceConnectionInfo,
  SSLConfigOptions,
} from './types';
import {
  showErrorNotification,
  showNotification,
  showSuccessNotification,
} from '../components/Services/Common/Notification';
import {
  addAllowedQueriesQuery,
  addAllowedQuery,
  addInheritedRole,
  addInsecureDomainQuery,
  allowedQueriesCollection,
  createAllowListQuery,
  deleteAllowedQueryQuery,
  deleteAllowListQuery,
  deleteDomain,
  deleteInheritedRole,
  getReloadCacheAndGetInconsistentObjectsQuery,
  getRemoteSchemaNameFromInconsistentObjects,
  getSourceFromInconistentObjects,
  reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery,
  updateAllowedQueryQuery,
  updateInheritedRole,
} from './utils';
import {
  fetchDataInit,
  makeMigrationCall,
  setConsistentSchema,
  UPDATE_CURRENT_DATA_SOURCE,
} from '../components/Services/Data/DataActions';
import { filterInconsistentMetadataObjects } from '../components/Services/Settings/utils';
import { clearIntrospectionSchemaCache } from '../components/Services/RemoteSchema/graphqlUtils';
import {
  createRESTEndpointQuery,
  dropInconsistentObjectsQuery,
  dropRESTEndpointQuery,
  exportMetadataQuery,
  generateReplaceMetadataQuery,
  inconsistentObjectsQuery,
  resetMetadataQuery,
} from './queryUtils';
import { currentDriver, dataSource, Driver, setDriver } from '../dataSources';
import { addSource, reloadSource, removeSource } from './sourcesUtils';
import { getDataSources } from './selector';
import { FixMe, ReduxState, Thunk } from '../types';
import {
  getConfirmation,
  isConsoleError,
  hashString,
} from '../components/Common/utils/jsUtils';
import _push from '../components/Services/Data/push';
import { dataSourceIsEqual } from '../components/Services/Data/DataSources/utils';
import { getSourceDriver } from '../components/Services/Data/utils';
import { hasuraToast } from '../new-components/Toasts';
import { createTextWithLinks } from './hyperlinkErrorMessageLink';

export interface ExportMetadataSuccess {
  type: 'Metadata/EXPORT_METADATA_SUCCESS';
  data:
    | {
        resource_version: number;
        metadata: HasuraMetadataV3;
      }
    | HasuraMetadataV3;
}
export interface ExportMetadataError {
  type: 'Metadata/EXPORT_METADATA_ERROR';
  data: string;
}
export interface ExportMetadataRequest {
  type: 'Metadata/EXPORT_METADATA_REQUEST';
}

export interface LoadInconsistentObjectsSuccess {
  type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_SUCCESS';
  data: InconsistentObject[];
}
export interface LoadInconsistentObjectsRequest {
  type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_REQUEST';
}
export interface LoadInconsistentObjectsError {
  type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_ERROR';
  data: string;
}

export interface DropInconsistentMetadataSuccess {
  type: 'Metadata/DROP_INCONSISTENT_METADATA_SUCCESS';
}
export interface DropInconsistentMetadataRequest {
  type: 'Metadata/DROP_INCONSISTENT_METADATA_REQUEST';
}
export interface DropInconsistentMetadataError {
  type: 'Metadata/DROP_INCONSISTENT_METADATA_ERROR';
  data: string;
}

export interface LoadAllowedQueries {
  type: 'Metadata/LOAD_ALLOWED_QUERIES';
  data: any[];
}
export interface AddAllowedQueries {
  type: 'Metadata/ADD_ALLOWED_QUERIES';
  data: any[];
}
export interface UpdateAllowedQuery {
  type: 'Metadata/UPDATE_ALLOWED_QUERY';
  data: {
    queryName: string;
    newQuery: { name: string; query: string; collection: string };
  };
}
export interface DeleteAllowedQuery {
  type: 'Metadata/DELETE_ALLOWED_QUERY';
  data: string;
}
export interface DeleteAllowList {
  type: 'Metadata/DELETE_ALLOW_LIST';
}

export interface AddDataSourceRequest {
  type: 'Metadata/ADD_DATA_SOURCE_REQUEST';
  data: {
    driver: Driver;
    payload: {
      name: string;
      dbUrl: string | { from_env: string };
      connection_pool_settings?: ConnectionPoolSettings;
      replace_configuration?: boolean;
      bigQuery: {
        projectId: string;
        datasets: string;
        global_select_limit: number;
      };
      connection_parameters?: ConnectionParams;
      sslConfiguration?: SSLConfigOptions;
      preparedStatements?: boolean;
      isolationLevel?: IsolationLevelOptions;
    };
  };
}
export interface AddDataSourceError {
  type: 'Metadata/ADD_DATA_SOURCE_ERROR';
  data: string;
}

export interface RemoveDataSourceRequest {
  type: 'Metadata/REMOVE_DATA_SOURCE_REQUEST';
  data: {
    driver: Driver;
    name: string;
  };
}
export interface RemoveDataSourceError {
  type: 'Metadata/REMOVE_DATA_SOURCE_ERROR';
  data: string;
}

export interface ReloadDataSourceRequest {
  type: 'Metadata/RELOAD_DATA_SOURCE_REQUEST';
  data: {
    driver: Driver;
    name: string;
  };
}
export interface ReloadDataSourceError {
  type: 'Metadata/RELOAD_DATA_SOURCE_ERROR';
  data: string;
}

export interface AddInheritedRole {
  type: 'Metadata/ADD_INHERITED_ROLE';
  data: {
    role_name: string;
    role_set: string[];
  };
}

export interface DeleteInheritedRole {
  type: 'Metadata/DELETE_INHERITED_ROLE';
  data: string;
}

export interface UpdateInheritedRole {
  type: 'Metadata/UPDATE_INHERITED_ROLE';
  data: {
    role_name: string;
    role_set: string[];
  };
}

export interface UpdateAPILimits {
  type: 'Metadata/UPDATE_API_LIMITS';
  data: {
    disabled: boolean;
    node_limit?: {
      global?: number;
      per_role?: Record<string, number>;
    };
    depth_limit?: {
      global?: number;
      per_role?: Record<string, number>;
    };
    batch_limit?: {
      global?: number;
      per_role?: Record<string, number>;
    };
  };
}

export type MetadataActions =
  | ExportMetadataSuccess
  | ExportMetadataError
  | ExportMetadataRequest
  | LoadInconsistentObjectsSuccess
  | LoadInconsistentObjectsRequest
  | LoadInconsistentObjectsError
  | DropInconsistentMetadataSuccess
  | DropInconsistentMetadataRequest
  | DropInconsistentMetadataError
  | LoadAllowedQueries
  | AddAllowedQueries
  | UpdateAllowedQuery
  | DeleteAllowList
  | DeleteAllowedQuery
  | AddDataSourceRequest
  | AddDataSourceError
  | RemoveDataSourceRequest
  | RemoveDataSourceError
  | ReloadDataSourceRequest
  | ReloadDataSourceError
  | AddInheritedRole
  | DeleteInheritedRole
  | UpdateInheritedRole
  | UpdateAPILimits
  | { type: typeof UPDATE_CURRENT_DATA_SOURCE; source: string };

export const exportMetadata =
  (
    successCb?: (data: MetadataResponse) => void,
    errorCb?: (err: string) => void
  ): Thunk<Promise<ReduxState | void>, MetadataActions> =>
  (dispatch, getState) => {
    const query = exportMetadataQuery;

    const options = {
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(query),
    };

    return dispatch(requestAction(Endpoints.metadata, options))
      .then(data => {
        dispatch({
          type: 'Metadata/EXPORT_METADATA_SUCCESS',
          data,
        });
        if (successCb) successCb(data);
        return getState();
      })
      .catch(err => {
        if (errorCb) errorCb(err);
      });
  };

const fetchDBEntities = (
  dataSourceName: string,
  headers: Record<string, string>,
  successHandler: (response: string[]) => void,
  errorCb?: (err: string) => void,
  noSupportCb?: () => void
) => {
  const dbTableNamesQuery = dataSource?.getDatabaseTableNames;

  if (!dbTableNamesQuery) {
    if (noSupportCb) noSupportCb();
    return;
  }

  const url = Endpoints.query;

  const query = getRunSqlQuery(dbTableNamesQuery, dataSourceName, false, true);

  Api.post<RunSQLSelectResponse>({
    url,
    headers,
    body: query,
  })
    .then(dbTablesResponse => {
      // sample: dbTablesResponse.result = [['table_name'], ['<table1>'], ['<table2>'], ...]
      const tableNames = dbTablesResponse.result
        .slice(1) // remove db results header
        .map((res: string[]) => res[0]) // extract table name from result array
        .sort(); // ensure order is maintained for consistent results

      successHandler(tableNames);
    })
    .catch(err => {
      trackRuntimeError(err);
      if (errorCb) errorCb(err);
    });
};

const makeConnectDBTelemetryEvent = (
  eventHandler: (event: ConnectDBEvent) => void,
  dbKind: Driver,
  dbEntities?: string[]
) => {
  // send entity_count only if DB entity data is available
  const connectDBEvent: ConnectDBEvent = {
    type: 'CONNECT_DB',
    data: {
      db_kind: dbKind,
      entity_count: dbEntities ? dbEntities.length : undefined,
      entity_hash: undefined,
    },
  };

  // set entity_hash only if non-zero entities exist
  if (dbEntities) {
    if (dbEntities.length) {
      const setEntityHashAndHandleEvent = (hash: string) => {
        connectDBEvent.data.entity_hash = hash;

        eventHandler(connectDBEvent);
      };

      hashString(dbEntities.toString()).then(entityHash =>
        setEntityHashAndHandleEvent(entityHash)
      );
    } else {
      // set fixed hash value for 0 entities
      connectDBEvent.data.entity_hash = '00000000000000000000000000000000';
      eventHandler(connectDBEvent);
    }
  } else {
    eventHandler(connectDBEvent);
  }
};

// TODO: move to some utils
const sendInitialDBStateTelemetry = (
  dataSourceName: string,
  headers: Record<string, string>
) => {
  const dbKind = currentDriver;

  const onDataFetch = (dbEntities: string[]) => {
    makeConnectDBTelemetryEvent(sendTelemetryEvent, dbKind, dbEntities);
  };

  const onNoDBSupport = () => {
    makeConnectDBTelemetryEvent(sendTelemetryEvent, dbKind);
  };

  fetchDBEntities(
    dataSourceName,
    headers,
    onDataFetch,
    undefined,
    onNoDBSupport
  );
};

export const addDataSource =
  (
    data: AddDataSourceRequest['data'],
    successCb?: () => void,
    replicas?: Omit<
      SourceConnectionInfo,
      | 'connection_string'
      | 'use_prepared_statements'
      | 'ssl_configuration'
      | 'isolation_level'
    >[],
    shouldShowNotifications = true
  ): Thunk<Promise<void | ReduxState>, MetadataActions> =>
  (dispatch, getState) => {
    const upQuery = addSource(data.driver, data.payload, replicas);
    const isEdit = data.payload.replace_configuration;
    const migrationName = isEdit ? `update_data_source` : `add_data_source`;
    const requestMsg = isEdit
      ? 'Updating data source...'
      : 'Adding data source...';
    const errorMsg = isEdit
      ? 'Updating data source failed'
      : 'Adding data source failed';

    const onSuccess = () => {
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: data.payload.name,
      });
      setDriver(data.driver);
      const onButtonClick = () => {
        if (data.payload.name)
          dispatch(_push(`/data/${data.payload.name}/schema`));
      };
      return dispatch(exportMetadata()).then(() => {
        dispatch(fetchDataInit(data.payload.name, data.driver));

        // send DB initial state to telemetry
        sendInitialDBStateTelemetry(data.payload.name, dataHeaders(getState));

        if (shouldShowNotifications) {
          dispatch(
            showNotification(
              {
                title: `Data source ${
                  !isEdit ? 'added' : 'updated'
                } successfully!`,
                level: 'success',
                autoDismiss: 0,
                alternateActionButtonProps: {
                  label: 'View Database',
                  onClick: onButtonClick,
                  trackId: 'data-tab-view-database-notification-button-add-db',
                },
              },
              'success'
            )
          );
        }
        if (successCb) successCb();
        return getState();
      });
    };

    const onError = (err: Record<string, any>) => {
      console.error(err);
      if (!isEdit) {
        dispatch(_push('/data/manage/connect'));
      }
      return err;
    };

    return makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      undefined,
      errorMsg,
      false,
      false,
      false,
      data.payload.name,
      shouldShowNotifications
    );
  };

export const renameDataSource =
  (
    data: AddDataSourceRequest['data'],
    successCb: () => void,
    renameData: {
      isRenameSource: boolean;
      name: string;
    },
    replicas?: Omit<
      SourceConnectionInfo,
      'connection_string' | 'use_prepared_statements' | 'isolation_level'
    >[]
  ): Thunk<Promise<void | ReduxState>, MetadataActions> =>
  (dispatch, getState) => {
    const { isRenameSource, name } = renameData;
    const { metadata } = getState();
    const { sources } = metadata.metadataObject ?? {};
    const currentSource = sources?.find(s => s.name === name);
    const isEdit = data.payload.replace_configuration;
    const addQuery = addSource(data.driver, data.payload, replicas);
    const renameQuery = {
      type: 'rename_source',
      args: {
        name,
        new_name: data.payload.name,
      },
    };

    const isOnlyRename = !!(
      currentSource &&
      dataSourceIsEqual(currentSource, addQuery.args) &&
      isRenameSource
    );

    const upQueries = [];
    upQueries.push(renameQuery);
    if (!isOnlyRename) upQueries.push(addQuery);

    const migrationName = `update_data_source`;
    const requestMsg = 'Updating data source...';
    const errorMsg = 'Updating data source failed';

    const onSuccess = () => {
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: data.payload.name,
      });
      setDriver(data.driver);
      const onButtonClick = () => {
        if (data.payload.name)
          dispatch(_push(`/data/${data.payload.name}/schema`));
      };
      return dispatch(exportMetadata())
        .then(() => {
          dispatch(fetchDataInit(data.payload.name, data.driver));
          dispatch(
            showNotification(
              {
                title: `Data source updated successfully!`,
                level: 'success',
                autoDismiss: 0,
                alternateActionButtonProps: {
                  label: 'View Database',
                  onClick: onButtonClick,
                  trackId:
                    'data-tab-view-database-notification-button-rename-db',
                },
              },
              'success'
            )
          );
          successCb();
          return getState();
        })
        .catch(console.error);
    };

    const onError = (err: Record<string, any>) => {
      console.error(err);
      if (!isEdit) {
        dispatch(_push('/data/manage/connect'));
      }
      return err;
    };

    return makeMigrationCall(
      dispatch,
      getState,
      upQueries,
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      undefined,
      errorMsg,
      true
    );
  };

export const removeDataSource =
  (
    data: RemoveDataSourceRequest['data']
  ): Thunk<Promise<void | ReduxState>, MetadataActions> =>
  (dispatch, getState) => {
    const { currentDataSource } = getState().tables;
    const sources = getDataSources(getState()).filter(
      s => s.name !== data.name
    );
    const upQuery = removeSource(data.driver, data.name);

    const migrationName = `remove_data_source`;
    const requestMsg = 'Removing data source...';
    const successMsg = 'Data source removed successfully';
    const errorMsg = 'Removing data source failed';

    const onSuccess = () => {
      if (currentDataSource === data.name) {
        dispatch({
          type: UPDATE_CURRENT_DATA_SOURCE,
          source: sources.length ? sources[0].name : '',
        });
      }
      dispatch(exportMetadata()).then(() => {
        const newSourceName = sources.length ? sources[0].name : '';
        if (newSourceName) {
          const driver = getSourceDriver(sources, newSourceName);
          setDriver(driver);
          dispatch(fetchDataInit(newSourceName, driver));
        }
      });
      return getState();
    };

    const onError = (err: Record<string, any>) => {
      console.error(err);
      return err;
    };

    return makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg,
      true
    );
  };

export const replaceMetadata =
  (
    newMetadata: HasuraMetadataV2 | MetadataResponse,
    successCb: () => void,
    errorCb: () => void
  ): Thunk<void, MetadataActions> =>
  (dispatch, getState) => {
    const exportSuccessCb = (oldMetadata: {
      resource_version: number;
      metadata: HasuraMetadataV3;
    }) => {
      const metadata =
        'version' in newMetadata ? newMetadata : newMetadata.metadata;
      const upQuery = generateReplaceMetadataQuery(metadata);
      const downQuery = generateReplaceMetadataQuery(oldMetadata.metadata);

      const migrationName = 'replace_metadata';

      const requestMsg = 'Importing metadata...';
      const successMsg = 'Metadata imported';
      const errorMsg = 'Failed importing metadata';

      const customOnSuccess = (
        response: [{ warnings: { code: string; message: string }[] }]
      ) => {
        if (successCb) successCb();
        const title = (code: string) => {
          if (code === 'illegal-event-trigger-name')
            return 'Rename Event Trigger Suggested';
          if (code === 'source-cleanup-failed') {
            return 'Manual Event Trigger Cleanup Needed';
          } else {
            return 'Time Limit Exceeded System Limit';
          }
        };
        response?.[0]?.warnings?.forEach(i => {
          hasuraToast({
            type: 'warning',
            title: title(i.code),
            children: createTextWithLinks(i.message),
            toastOptions: {
              duration: Infinity,
            },
          });
        });

        const updateCurrentDataSource = (newState: MetadataResponse) => {
          const currentSource = newState.metadata.sources.find(
            (x: MetadataDataSource) =>
              x.name === getState().tables.currentDataSource
          );

          if (!currentSource && newState.metadata.sources?.[0]) {
            dispatch({
              type: UPDATE_CURRENT_DATA_SOURCE,
              source: newState.metadata.sources[0].name,
            });
            setDriver(newState.metadata.sources[0].kind ?? 'postgres');
            dispatch(
              fetchDataInit(
                newState.metadata.sources[0].name,
                newState.metadata.sources[0].kind
              )
            );
          }
        };

        const onError = (err: string) => {
          dispatch(
            showErrorNotification('Metadata reset failed', null, { error: err })
          );
        };

        dispatch(exportMetadata(updateCurrentDataSource, onError));
      };
      const customOnError = () => {
        if (errorCb) errorCb();
      };

      makeMigrationCall(
        dispatch,
        getState,
        [upQuery],
        [downQuery],
        migrationName,
        customOnSuccess,
        customOnError,
        requestMsg,
        successMsg,
        errorMsg,
        true
      );
    };

    const exportErrorCb = () => {
      if (errorCb) errorCb();

      dispatch(
        showErrorNotification(
          'Metadata import failed',
          'Failed to get the existing metadata from the server'
        )
      );
    };

    dispatch(exportMetadata(exportSuccessCb, exportErrorCb));
  };

export const resetMetadata =
  (
    successCb: () => void,
    errorCb: (err: string) => void
  ): Thunk<void, MetadataActions> =>
  (dispatch, getState) => {
    const headers = dataHeaders(getState);

    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: headers || {},
      body: JSON.stringify(resetMetadataQuery),
    };

    return dispatch(
      requestAction(Endpoints.metadata, options as RequestInit)
    ).then(
      (response: { warnings: { code: string; message: string }[] }) => {
        dispatch({ type: UPDATE_CURRENT_DATA_SOURCE, source: '' });
        dispatch(exportMetadata());
        if (successCb) successCb();

        response?.warnings?.forEach(i => {
          hasuraToast({
            type: 'warning',
            title: 'Manual Event Trigger Cleanup Needed',
            children: createTextWithLinks(i.message),
            toastOptions: {
              duration: Infinity,
            },
          });
        });

        hasuraToast({ title: 'Metadata reset successfully!', type: 'success' });
      },
      error => {
        console.error(error);
        dispatch(showErrorNotification('Metadata reset failed', null, error));
        if (errorCb) {
          errorCb(error);
        }
      }
    );
  };

export const replaceMetadataFromFile =
  (
    fileContent: string,
    successCb: () => void,
    errorCb: () => void
  ): Thunk<void, MetadataActions> =>
  dispatch => {
    let parsedFileContent;
    try {
      parsedFileContent = JSON.parse(fileContent);
    } catch (e) {
      if (isConsoleError(e)) {
        dispatch(
          showErrorNotification('Error parsing metadata file', e.toString())
        );
      }

      if (errorCb) errorCb();

      return;
    }

    const onSuccess = () => {
      if (successCb) successCb();
    };

    const onError = () => {
      if (errorCb) errorCb();
    };

    dispatch(replaceMetadata(parsedFileContent, onSuccess, onError));
  };

const handleInconsistentObjects = (
  inconsistentObjects: any[]
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const allSchemas = getState().tables.allSchemas;

    dispatch({
      type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_SUCCESS',
      data: inconsistentObjects,
    });

    if (inconsistentObjects.length > 0) {
      const filteredSchema = filterInconsistentMetadataObjects(
        allSchemas,
        inconsistentObjects,
        'tables'
      );

      dispatch(setConsistentSchema(filteredSchema) as FixMe);
    }
  };
};

export const loadInconsistentObjects = (
  reloadConfig: {
    shouldReloadMetadata?: boolean;
    shouldReloadRemoteSchemas?: boolean;
    shouldReloadAllSources?: boolean;
  },
  successCb?: () => void,
  failureCb?: (error: string) => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const inconsistentObjectsInMetadata =
      getState().metadata.inconsistentObjects;

    const inconsistentSources = getSourceFromInconistentObjects(
      inconsistentObjectsInMetadata
    );
    const inconsistentRemoteSchemas =
      getRemoteSchemaNameFromInconsistentObjects(inconsistentObjectsInMetadata);

    const headers = dataHeaders(getState);
    const source = getState().tables.currentDataSource;
    const {
      shouldReloadMetadata,
      shouldReloadRemoteSchemas,
      shouldReloadAllSources,
    } = reloadConfig;

    let reloadSources: string[] | boolean = [];
    if (shouldReloadAllSources) {
      reloadSources = true;
    } else if (inconsistentSources.length) {
      reloadSources = inconsistentSources;
    }

    let reloadRemoteSchemas: string[] | boolean = [];
    if (shouldReloadRemoteSchemas) {
      reloadRemoteSchemas = true;
    } else if (inconsistentRemoteSchemas.length) {
      reloadRemoteSchemas = inconsistentRemoteSchemas;
    }

    const loadQuery = shouldReloadMetadata
      ? getReloadCacheAndGetInconsistentObjectsQuery(
          reloadRemoteSchemas,
          source,
          reloadSources
        )
      : inconsistentObjectsQuery;

    dispatch({ type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_REQUEST' });
    return dispatch(
      requestAction(Endpoints.metadata, {
        method: 'POST',
        headers,
        body: JSON.stringify(loadQuery),
      })
    ).then(
      data => {
        const inconsistentObjects = shouldReloadMetadata
          ? data[1].inconsistent_objects
          : data.inconsistent_objects;

        dispatch(handleInconsistentObjects(inconsistentObjects));

        if (successCb) {
          successCb();
        }
        if (shouldReloadRemoteSchemas) {
          clearIntrospectionSchemaCache();
        }
      },
      error => {
        console.error(error);
        dispatch({
          type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_ERROR',
          data: error,
        });
        if (failureCb) {
          failureCb(error);
        }
      }
    );
  };
};
export const reloadDataSource =
  (
    data: ReloadDataSourceRequest['data']
  ): Thunk<Promise<void | ReduxState>, MetadataActions> =>
  (dispatch, getState) => {
    const query = reloadSource(data.name);

    const options = {
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(query),
    };

    return dispatch(requestAction(Endpoints.metadata, options))
      .then(() => {
        dispatch(showSuccessNotification('Data source reloaded successfully!'));
        dispatch(exportMetadata());
        dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
        return getState();
      })
      .catch(err => {
        console.error(err);
        dispatch(showErrorNotification('Reload data source failed', null, err));
      });
  };
export const reloadRemoteSchema = (
  remoteSchemaName: string,
  successCb: () => void,
  failureCb: (err: string) => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const headers = dataHeaders(getState);
    const source = getState().tables.currentDataSource;

    const reloadQuery = reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery(
      remoteSchemaName,
      source
    );

    dispatch({ type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_REQUEST' });
    return dispatch(
      requestAction(Endpoints.metadata, {
        method: 'POST',
        headers,
        body: JSON.stringify(reloadQuery),
      })
    ).then(
      data => {
        const inconsistentObjects = data[1].inconsistent_objects;

        dispatch(handleInconsistentObjects(inconsistentObjects));

        clearIntrospectionSchemaCache();

        if (successCb) {
          successCb();
        }
      },
      error => {
        console.error(error);
        dispatch({
          type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_ERROR',
          data: error,
        });
        if (failureCb) {
          failureCb(error);
        }
      }
    );
  };
};

export const reloadMetadata = (
  shouldReloadRemoteSchemas: boolean,
  shouldReloadAllSources: boolean,
  successCb: () => void,
  failureCb: () => void
): Thunk<void, MetadataActions> => {
  return dispatch => {
    return dispatch(
      loadInconsistentObjects(
        {
          shouldReloadMetadata: true,
          shouldReloadRemoteSchemas,
          shouldReloadAllSources,
        },
        successCb,
        failureCb
      )
    );
  };
};

export const dropInconsistentObjects = (
  successCb: () => void,
  failureCb: () => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const headers = dataHeaders(getState);
    dispatch({ type: 'Metadata/DROP_INCONSISTENT_METADATA_REQUEST' });
    return dispatch(
      requestAction(Endpoints.metadata, {
        method: 'POST',
        headers,
        body: JSON.stringify(dropInconsistentObjectsQuery),
      })
    ).then(
      () => {
        dispatch({ type: 'Metadata/DROP_INCONSISTENT_METADATA_SUCCESS' });
        dispatch(showSuccessNotification('Dropped inconsistent metadata'));
        dispatch(loadInconsistentObjects({ shouldReloadRemoteSchemas: false }));
        clearIntrospectionSchemaCache();
        if (successCb) {
          successCb();
        }
      },
      error => {
        console.error(error);
        dispatch({
          type: 'Metadata/DROP_INCONSISTENT_METADATA_ERROR',
          data: error,
        });
        dispatch(
          showErrorNotification(
            'Dropping inconsistent metadata failed',
            null,
            error
          )
        );
        if (failureCb) {
          failureCb();
        }
      }
    );
  };
};

export const updateAllowedQuery = (
  queryName: string,
  newQuery: { name: string; query: string },
  collectionName: string
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = updateAllowedQueryQuery(
      queryName,
      newQuery,
      collectionName
    );

    const migrationName = `update_allowed_query`;
    const requestMsg = 'Updating allowed query...';
    const successMsg = 'Updated allow-list query';
    const errorMsg = 'Updating allow-list query failed';

    const updatedQuery = { ...newQuery, collection: collectionName };

    const onSuccess = () => {
      dispatch({
        type: 'Metadata/UPDATE_ALLOWED_QUERY',
        data: { queryName, newQuery: updatedQuery },
      });
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const deleteAllowedQuery = (
  queryName: string,
  isLastQuery: boolean,
  collectionName: string
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = isLastQuery
      ? deleteAllowListQuery(collectionName)
      : deleteAllowedQueryQuery(queryName, collectionName);

    const migrationName = `delete_allowed_query`;
    const requestMsg = 'Deleting allowed query...';
    const successMsg = 'Deleted query from allow-list';
    const errorMsg = 'Deleting query from allow-list failed';

    const onSuccess = () => {
      dispatch({ type: 'Metadata/DELETE_ALLOWED_QUERY', data: queryName });
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const deleteAllowList = (
  collectionNames: string[]
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQueries: {
      type: string;
      args: { collection: string; cascade: boolean };
    }[] = [];

    collectionNames.forEach(collectionName => {
      upQueries.push(deleteAllowListQuery(collectionName));
    });

    const migrationName = 'delete_allow_list';
    const requestMsg = 'Deleting allow list...';
    const successMsg = 'Deleted all queries from allow-list';
    const errorMsg = 'Deleting queries from allow-list failed';

    const onSuccess = () => {
      dispatch({ type: 'Metadata/DELETE_ALLOW_LIST' });
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      upQueries,
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const addAllowedQueries = (
  queries: Array<{ name: string; query: string }>,
  callback: any
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    if (queries.length === 0) {
      dispatch(showErrorNotification('No queries found'));

      return;
    }

    const source = getState().tables.currentDataSource;
    const isAllowedQueryCollectionInMetadata =
      getState().metadata?.metadataObject?.query_collections?.find(
        qc => qc.name === allowedQueriesCollection
      ) ?? false;

    const upQuery = !isAllowedQueryCollectionInMetadata
      ? createAllowListQuery(queries, source)
      : addAllowedQueriesQuery(queries, source);

    const migrationName = `add_allowed_queries`;
    const requestMsg = 'Adding allowed queries...';
    const successMsg = `${
      queries.length > 1 ? 'Queries' : 'Query'
    } added to allow-list`;
    const errorMsg = 'Adding query to allow-list failed';

    const onSuccess = () => {
      dispatch({ type: 'Metadata/ADD_ALLOWED_QUERIES', data: queries });
      callback();
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const addInsecureDomain = (
  host: string,
  port: string,
  callback: any
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    if (!host.trim().length) {
      dispatch(showErrorNotification('No domain found'));

      return;
    }
    const upQuery = addInsecureDomainQuery(host, port);
    const migrationName = `add_insecure_tls_domains`;
    const requestMsg = 'Adding domain to insecure TLS allow list...';
    const successMsg = `Domain added to insecure TLS allow list successfully`;
    const errorMsg = 'Adding domain to insecure TLS allow list failed';

    const onSuccess = () => {
      dispatch(exportMetadata());
      callback();
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const deleteInsecureDomain = (
  host: string,
  port?: string
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = deleteDomain(host, port);
    const migrationName = `delete_insecure_domain`;
    const requestMsg = 'Deleting Insecure domain...';
    const successMsg = 'Domain deleted!';
    const errorMsg = 'Deleting domain failed!';

    const onSuccess = () => {
      dispatch(exportMetadata());
    };

    const onError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const addInheritedRoleAction = (
  role_name: string,
  role_set: string[],
  callback?: () => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = addInheritedRole(role_name, role_set);

    const migrationName = `add_inherited_role`;
    const requestMsg = 'Adding inherited role...';
    const successMsg = 'Added inherited role';
    const errorMsg = 'Adding inherited role failed';

    const onSuccess = () => {
      dispatch({
        type: 'Metadata/ADD_INHERITED_ROLE',
        data: { role_name, role_set },
      });
      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
      if (callback) callback();
    };

    const onError = () => {
      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
    };

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const deleteInheritedRoleAction = (
  role_name: string,
  callback?: () => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = deleteInheritedRole(role_name);

    const migrationName = `delete_inherited_role`;
    const requestMsg = 'Deleting inherited role...';
    const successMsg = 'Deleted inherited role';
    const errorMsg = 'Deleting inherited role failed';

    const onSuccess = () => {
      dispatch({ type: 'Metadata/DELETE_INHERITED_ROLE', data: role_name });
      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
      if (callback) {
        callback();
      }
    };

    const onError = () => {
      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
    };

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const updateInheritedRoleAction = (
  role_name: string,
  role_set: string[],
  callback?: () => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = updateInheritedRole(role_name, role_set);

    const migrationName = `update_inherited_role`;
    const requestMsg = 'Updating inherited role...';
    const successMsg = 'Updated inherited role';
    const errorMsg = 'Updating inherited role failed';

    const onSuccess = () => {
      dispatch({
        type: 'Metadata/UPDATE_INHERITED_ROLE',
        data: { role_name, role_set },
      });
      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
      if (callback) {
        callback();
      }
    };

    const onError = () => {
      dispatch(loadInconsistentObjects({ shouldReloadMetadata: false }));
    };

    makeMigrationCall(
      dispatch,
      getState,
      [upQuery],
      undefined,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const addRESTEndpoint =
  (
    queryObj: RestEndpointEntry,
    request: string,
    successCb?: () => void,
    errorCb?: () => void
  ): Thunk<Promise<void | ReduxState>, MetadataActions> =>
  (dispatch, getState) => {
    const { currentDataSource } = getState().tables;
    const { metadataObject } = getState().metadata;
    const upQueries = [];
    const downQueries = [];

    const consoleCollection = metadataObject?.query_collections?.find(
      collection => collection.name === allowedQueriesCollection
    );

    if (!consoleCollection) {
      upQueries.push(
        ...createAllowListQuery(
          [{ name: queryObj.name, query: request }],
          currentDataSource
        ).args
      );
      downQueries.push(deleteAllowListQuery().args);
    } else {
      upQueries.push(addAllowedQuery({ name: queryObj.name, query: request }));
      downQueries.push(deleteAllowedQueryQuery(queryObj.name));
    }

    // the REST endpoint based requests
    const upQuery = createRESTEndpointQuery(queryObj);
    const downQuery = dropRESTEndpointQuery(queryObj.url);

    upQueries.push(upQuery);
    downQueries.push(downQuery);

    const migrationName = `create_rest_endpoint_${queryObj.url}`;
    const requestMsg = `Creating REST endpoint ${queryObj.name}`;
    const successMsg = 'Successfully created REST endpoint';
    const errorMsg = 'Error creating REST endpoint';

    const onSuccess = () => {
      if (successCb) {
        successCb();
      }
      dispatch(exportMetadata());
    };
    const onError = () => {
      if (errorCb) {
        errorCb();
      }
    };

    return makeMigrationCall(
      dispatch,
      getState,
      upQueries,
      downQueries,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };

export const dropRESTEndpoint =
  (
    endpointName: string,
    request: string,
    successCb?: () => void,
    errorCb?: () => void
  ): Thunk<Promise<void | ReduxState>, MetadataActions> =>
  (dispatch, getState) => {
    const currentRESTEndpoints =
      getState().metadata.metadataObject?.rest_endpoints;

    if (!currentRESTEndpoints) {
      dispatch(
        showErrorNotification(
          "Deletion of REST endpoint isn't possible.",
          'Your metadata seems to be empty!'
        )
      );
      return;
    }

    const currentObj = currentRESTEndpoints.find(
      re => re.name === endpointName
    );

    if (!currentObj) {
      dispatch(
        showErrorNotification(
          "Deletion of REST endpoint isn't possible.",
          `We could not find the endpoint ${endpointName} to delete`
        )
      );
      return;
    }

    const confirmation = getConfirmation(
      `You want to delete the endpoint: ${endpointName}`
    );

    if (!confirmation) {
      return;
    }

    const upQueries = [
      dropRESTEndpointQuery(endpointName),
      deleteAllowedQueryQuery(endpointName),
    ];
    const downQueries = [
      addAllowedQuery({ name: endpointName, query: request }),
      createRESTEndpointQuery(currentObj),
    ];

    const migrationName = `drop_rest_endpoint_${endpointName}`;
    const requestMsg = `Dropping REST endpoint ${currentObj.name}`;
    const successMsg = 'Successfully dropped REST endpoint';
    const errorMsg = 'Error dropping REST endpoint';

    const onSuccess = () => {
      if (successCb) {
        successCb();
      }
      dispatch(exportMetadata());
    };
    const onError = () => {
      if (errorCb) {
        errorCb();
      }
    };

    return makeMigrationCall(
      dispatch,
      getState,
      upQueries,
      downQueries,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };

export const editRESTEndpoint =
  (
    oldQueryObj: RestEndpointEntry,
    newQueryObj: RestEndpointEntry,
    request: string,
    successCb?: () => void,
    errorCb?: () => void
  ): Thunk<Promise<void | ReduxState>, MetadataActions> =>
  (dispatch, getState) => {
    const currentEndpoints = getState().metadata.metadataObject?.rest_endpoints;

    if (!currentEndpoints) {
      dispatch(
        showErrorNotification(
          "Editing this REST endpoint isn't possible.",
          'Your metadata seems to be empty!'
        )
      );
      return;
    }

    const dropOldQueryFromCollection = deleteAllowedQueryQuery(
      oldQueryObj.name
    );
    const addNewQueryToCollection = addAllowedQuery({
      name: newQueryObj.name,
      query: request,
    });

    const dropNewQueryFromCollection = deleteAllowedQueryQuery(
      newQueryObj.name
    );
    const addOldQueryToCollection = addAllowedQuery({
      name: oldQueryObj.name,
      query: request,
    });

    const upQueries = [
      dropRESTEndpointQuery(oldQueryObj.name),
      dropOldQueryFromCollection,
      addNewQueryToCollection,
      createRESTEndpointQuery(newQueryObj),
    ];

    const downQueries = [
      dropRESTEndpointQuery(newQueryObj.name),
      dropNewQueryFromCollection,
      addOldQueryToCollection,
      createRESTEndpointQuery(oldQueryObj),
    ];

    const migrationName = `edit_rest_endpoint_${newQueryObj.url}_${newQueryObj.name}`;
    const requestMsg = `Editing REST endpoint ${oldQueryObj.name}`;
    const successMsg = 'Successfully edited REST endpoint';
    const errorMsg = 'Error editing REST endpoint';

    const onSuccess = () => {
      if (successCb) {
        successCb();
      }
      dispatch(exportMetadata());
    };
    const onError = () => {
      if (errorCb) {
        errorCb();
      }
      dispatch(exportMetadata());
    };

    return makeMigrationCall(
      dispatch,
      getState,
      upQueries,
      downQueries,
      migrationName,
      onSuccess,
      onError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
