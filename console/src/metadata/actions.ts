import requestAction from '../utils/requestAction';
import Endpoints, { globalCookiePolicy } from '../Endpoints';
import { HasuraMetadataV2, HasuraMetadataV3, RestEndpointEntry } from './types';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../components/Services/Common/Notification';
import {
  deleteAllowListQuery,
  deleteAllowedQueryQuery,
  createAllowListQuery,
  addAllowedQueriesQuery,
  getReloadCacheAndGetInconsistentObjectsQuery,
  reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery,
  updateAllowedQueryQuery,
  allowedQueriesCollection,
  addAllowedQuery,
} from './utils';
import {
  makeMigrationCall,
  setConsistentSchema,
  UPDATE_CURRENT_DATA_SOURCE,
  fetchDataInit,
} from '../components/Services/Data/DataActions';
import { filterInconsistentMetadataObjects } from '../components/Services/Settings/utils';
import { clearIntrospectionSchemaCache } from '../components/Services/RemoteSchema/graphqlUtils';
import {
  inconsistentObjectsQuery,
  dropInconsistentObjectsQuery,
  exportMetadataQuery,
  generateReplaceMetadataQuery,
  resetMetadataQuery,
  createRESTEndpointQuery,
  dropRESTEndpointQuery,
} from './queryUtils';
import { Driver, setDriver } from '../dataSources';
import { addSource, removeSource, reloadSource } from './sourcesUtils';
import { getDataSources } from './selector';
import { FixMe, ReduxState, Thunk } from '../types';
import { getConfirmation } from '../components/Common/utils/jsUtils';
import _push from '../components/Services/Data/push';

export interface ExportMetadataSuccess {
  type: 'Metadata/EXPORT_METADATA_SUCCESS';
  data: HasuraMetadataV3;
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
  data: any;
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
    newQuery: { name: string; query: string };
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
      connection_pool_settings: {
        max_connections?: number;
        idle_timeout?: number; // in seconds
        retries?: number;
      };
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

export interface AddRestEndpoint {
  type: 'Metadata/ADD_REST_ENDPOINT';
  data: RestEndpointEntry[];
}

export interface DropRestEndpoint {
  type: 'Metadata/DROP_REST_ENDPOINT';
  data: RestEndpointEntry[];
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
  | AddRestEndpoint
  | DropRestEndpoint
  | { type: typeof UPDATE_CURRENT_DATA_SOURCE; source: string };

export const exportMetadata = (
  successCb?: (data: HasuraMetadataV2) => void,
  errorCb?: (err: string) => void
): Thunk<Promise<ReduxState | void>, MetadataActions> => (
  dispatch,
  getState
) => {
  const { dataHeaders } = getState().tables;

  const query = exportMetadataQuery;

  const options = {
    method: 'POST',
    headers: dataHeaders,
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

export const addDataSource = (
  data: AddDataSourceRequest['data'],
  successCb: () => void,
  skipNotification = false
): Thunk<Promise<void | ReduxState>, MetadataActions> => (
  dispatch,
  getState
) => {
  const { dataHeaders } = getState().tables;

  const query = addSource(data.driver, data.payload);

  const options = {
    method: 'POST',
    headers: dataHeaders,
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(Endpoints.metadata, options))
    .then(() => {
      successCb();
      if (!skipNotification) {
        dispatch(showSuccessNotification('Data source added successfully!'));
      }
      dispatch(exportMetadata());
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: data.payload.name,
      });
      setDriver(data.driver);
      dispatch(fetchDataInit(data.payload.name, data.driver));
      return getState();
    })
    .catch(err => {
      console.error(err);
      dispatch(_push('/data/manage/connect'));
      if (!skipNotification) {
        dispatch(showErrorNotification('Add data source failed', null, err));
      }
      return err;
    });
};

export const removeDataSource = (
  data: RemoveDataSourceRequest['data'],
  skipNotification = false
): Thunk<Promise<void | ReduxState>, MetadataActions> => (
  dispatch,
  getState
) => {
  const { dataHeaders, currentDataSource } = getState().tables;
  const sources = getDataSources(getState()).filter(s => s.name !== data.name);

  const query = removeSource(data.driver, data.name);

  const options = {
    method: 'POST',
    headers: dataHeaders,
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(Endpoints.metadata, options))
    .then(() => {
      if (currentDataSource === data.name) {
        dispatch({
          type: UPDATE_CURRENT_DATA_SOURCE,
          source: sources.length ? sources[0].name : '',
        });
      }
      if (!skipNotification) {
        dispatch(showSuccessNotification('Data source removed successfully!'));
      }
      dispatch(exportMetadata());
      return getState();
    })
    .catch(err => {
      console.error(err);
      if (!skipNotification) {
        dispatch(showErrorNotification('Remove data source failed', null, err));
      }
      return err;
    });
};

export const editDataSource = (
  oldName: string | undefined,
  data: AddDataSourceRequest['data'],
  onSuccessCb: () => void
): Thunk<Promise<void | ReduxState>, MetadataActions> => dispatch => {
  return dispatch(
    removeDataSource(
      { driver: data.driver, name: oldName ?? data.payload.name },
      true
    )
  )
    .then(() => {
      // FIXME?: There might be a problem when or if the metadata is inconsistent,
      // we should be providing a better error message for the same
      dispatch(
        addDataSource(
          data,
          () => {
            dispatch(
              showSuccessNotification(
                'Successfully updated datasource details.'
              )
            );
            onSuccessCb();
          },
          true
        )
      ).catch(err => {
        console.error(err);
        dispatch(
          showErrorNotification(
            'Failed to edit data source',
            'There was a problem in editing the details of the datasource'
          )
        );
      });
    })
    .catch(err => {
      console.error(err);
      dispatch(
        showErrorNotification(
          'Failed to edit data source',
          'There was a problem in editing the details of the datasource'
        )
      );
    });
};

export const reloadDataSource = (
  data: ReloadDataSourceRequest['data']
): Thunk<Promise<void | ReduxState>, MetadataActions> => (
  dispatch,
  getState
) => {
  const { dataHeaders } = getState().tables;

  const query = reloadSource(data.name);

  const options = {
    method: 'POST',
    headers: dataHeaders,
    body: JSON.stringify(query),
  };

  return dispatch(requestAction(Endpoints.metadata, options))
    .then(() => {
      dispatch(showSuccessNotification('Data source reloaded successfully!'));
      dispatch(exportMetadata());
      return getState();
    })
    .catch(err => {
      console.error(err);
      dispatch(showErrorNotification('Reload data source failed', null, err));
    });
};

export const replaceMetadata = (
  newMetadata: HasuraMetadataV2,
  successCb: () => void,
  errorCb: () => void
): Thunk<void, MetadataActions> => (dispatch, getState) => {
  const exportSuccessCb = (oldMetadata: HasuraMetadataV2) => {
    const upQuery = generateReplaceMetadataQuery(newMetadata);
    const downQuery = generateReplaceMetadataQuery(oldMetadata);

    const migrationName = 'replace_metadata';

    const requestMsg = 'Importing metadata...';
    const successMsg = 'Metadata imported';
    const errorMsg = 'Failed importing metadata';

    const customOnSuccess = () => {
      if (successCb) successCb();
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
      errorMsg
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

export const resetMetadata = (
  successCb: () => void,
  errorCb: (err: string) => void
): Thunk<void, MetadataActions> => (dispatch, getState) => {
  const headers = getState().tables.dataHeaders;

  const options = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: headers || {},
    body: JSON.stringify(resetMetadataQuery),
  };

  return dispatch(
    requestAction(Endpoints.metadata, options as RequestInit)
  ).then(
    () => {
      dispatch(exportMetadata());
      if (successCb) {
        successCb();
      }
      dispatch(showSuccessNotification('Metadata reset successfully!'));
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

export const replaceMetadataFromFile = (
  fileContent: string,
  successCb: () => void,
  errorCb: () => void
): Thunk<void, MetadataActions> => dispatch => {
  let parsedFileContent;
  try {
    parsedFileContent = JSON.parse(fileContent);
  } catch (e) {
    dispatch(
      showErrorNotification('Error parsing metadata file', e.toString())
    );

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
  },
  successCb?: () => void,
  failureCb?: (error: string) => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const headers = getState().tables.dataHeaders;
    const source = getState().tables.currentDataSource;
    const { shouldReloadMetadata, shouldReloadRemoteSchemas } = reloadConfig;

    const loadQuery = shouldReloadMetadata
      ? getReloadCacheAndGetInconsistentObjectsQuery(
          !!shouldReloadRemoteSchemas,
          source
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

export const reloadRemoteSchema = (
  remoteSchemaName: string,
  successCb: () => void,
  failureCb: (err: string) => void
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const headers = getState().tables.dataHeaders;
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
  successCb: () => void,
  failureCb: () => void
): Thunk<void, MetadataActions> => {
  return dispatch => {
    return dispatch(
      loadInconsistentObjects(
        {
          shouldReloadMetadata: true,
          shouldReloadRemoteSchemas,
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
    const headers = getState().tables.dataHeaders;
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
  newQuery: { name: string; query: string }
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = updateAllowedQueryQuery(queryName, newQuery);

    const migrationName = `update_allowed_query`;
    const requestMsg = 'Updating allowed query...';
    const successMsg = 'Updated allow-list query';
    const errorMsg = 'Updating allow-list query failed';

    const onSuccess = () => {
      dispatch({
        type: 'Metadata/UPDATE_ALLOWED_QUERY',
        data: { queryName, newQuery },
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
  isLastQuery: boolean
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = isLastQuery
      ? deleteAllowListQuery()
      : deleteAllowedQueryQuery(queryName);

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

export const deleteAllowList = (): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const upQuery = deleteAllowListQuery();
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

export const addRESTEndpoint = (
  queryObj: RestEndpointEntry,
  request: string,
  successCb?: () => void,
  errorCb?: () => void
): Thunk<Promise<void | ReduxState>, MetadataActions> => (
  dispatch,
  getState
) => {
  const { currentDataSource } = getState().tables;
  const { rest_endpoints, metadataObject } = getState().metadata;
  let currentEndpoints: RestEndpointEntry[] = [];
  if (rest_endpoints?.length) {
    currentEndpoints = rest_endpoints;
  }
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
    dispatch({
      type: 'Metadata/ADD_REST_ENDPOINT',
      data: [...currentEndpoints, queryObj],
    });
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

export const dropRESTEndpoint = (
  endpointName: string,
  request: string,
  successCb?: () => void,
  errorCb?: () => void
): Thunk<Promise<void | ReduxState>, MetadataActions> => (
  dispatch,
  getState
) => {
  const currentRESTEndpoints = getState().metadata.metadataObject
    ?.rest_endpoints;

  if (!currentRESTEndpoints) {
    dispatch(
      showErrorNotification(
        "Deletion of REST endpoint isn't possible.",
        'Your metadata seems to be empty!'
      )
    );
    return;
  }

  const currentObj = currentRESTEndpoints.find(re => re.name === endpointName);

  if (!currentObj) {
    dispatch(
      showErrorNotification(
        "Deletion of REST endpoint isn't possible.",
        `We could not find the endpoint ${endpointName} to delete`
      )
    );
    return;
  }

  const filteredEndpoints = currentRESTEndpoints.filter(
    re => re.name !== endpointName
  );

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
    dispatch({
      type: 'Metadata/DROP_REST_ENDPOINT',
      data: filteredEndpoints,
    });
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

export const editRESTEndpoint = (
  oldQueryObj: RestEndpointEntry,
  newQueryObj: RestEndpointEntry,
  request: string,
  successCb?: () => void,
  errorCb?: () => void
): Thunk<Promise<void | ReduxState>, MetadataActions> => (
  dispatch,
  getState
) => {
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

  // using `any` here since the 3 queries have a different
  // return types, hence ended up using any
  let upQueries: any = [
    dropRESTEndpointQuery(oldQueryObj.name),
    createRESTEndpointQuery(newQueryObj),
  ];
  let downQueries: any = [
    createRESTEndpointQuery(oldQueryObj),
    dropRESTEndpointQuery(newQueryObj.name),
  ];

  if (newQueryObj.name !== oldQueryObj.name) {
    const newAllowedQuery = addAllowedQuery({
      name: newQueryObj.name,
      query: request,
    });
    const deleteOldFromCollection = deleteAllowedQueryQuery(oldQueryObj.name);
    const addOldIntoQueryCollection = addAllowedQuery({
      name: oldQueryObj.name,
      query: request,
    });
    const newDeleteAllowedQuery = deleteAllowedQueryQuery(newQueryObj.name);
    upQueries = [
      upQueries[0],
      deleteOldFromCollection,
      newAllowedQuery,
      upQueries[1],
    ];
    downQueries = [
      addOldIntoQueryCollection,
      downQueries[0],
      downQueries[1],
      newDeleteAllowedQuery,
    ];
  }

  const migrationName = `edit_rest_endpoint_${newQueryObj.url}_${newQueryObj.name}`;
  const requestMsg = `Editing REST endpoint ${oldQueryObj.name}`;
  const successMsg = 'Successfully edited REST endpoint';
  const errorMsg = 'Error editing REST endpoint';

  const filteredEndpoints = currentEndpoints.filter(
    re => re.name !== oldQueryObj.name
  );

  const onSuccess = () => {
    dispatch({
      type: 'Metadata/ADD_REST_ENDPOINT',
      data: [...filteredEndpoints, newQueryObj],
    });
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
