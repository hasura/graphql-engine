import { Thunk } from '../types';
import {
  exportMetadataQuery,
  generateReplaceMetadataQuery,
  resetMetadataQuery,
  inconsistentObjectsQuery,
  dropInconsistentObjectsQuery,
} from '../components/Common/utils/v1QueryUtils';
import requestAction from '../utils/requestAction';
import Endpoints, { globalCookiePolicy } from '../Endpoints';
import { HasuraMetadataV2 } from './types';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../components/Services/Common/Notification';
import {
  deleteAllowListQuery,
  deleteAllowedQueryQuery,
  createAllowListQuery,
  addAllowedQueriesQuery,
  loadAllowedQueriesQuery,
  getReloadCacheAndGetInconsistentObjectsQuery,
  reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery,
  updateAllowedQueryQuery,
} from './utils';
import {
  makeMigrationCall,
  setConsistentSchema,
  setConsistentFunctions,
} from '../components/Services/Data/DataActions';
import { filterInconsistentMetadataObjects } from '../components/Services/Settings/utils';
import { setConsistentRemoteSchemas } from '../components/Services/RemoteSchema/Actions';
import { setActions } from '../components/Services/Actions/reducer';
import { clearIntrospectionSchemaCache } from '../components/Services/RemoteSchema/graphqlUtils';

export interface ExportMetadataSuccess {
  type: 'Metadata/EXPORT_METADATA_SUCCESS';
  data: HasuraMetadataV2;
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
  | DeleteAllowedQuery;

export const exportMetadata = (
  successCb: (data: HasuraMetadataV2) => void,
  errorCb: (err: string) => void
): Thunk<void, MetadataActions> => (dispatch, getState) => {
  const { dataHeaders } = getState().tables;

  const query = exportMetadataQuery;

  const options = {
    method: 'POST',
    headers: {
      ...dataHeaders,
    },
    body: JSON.stringify(query),
  };

  dispatch(requestAction(Endpoints.query, options))
    .then(data => {
      successCb(data);
      dispatch({
        type: 'Metadata/EXPORT_METADATA_SUCCESS',
        data,
      });
    })
    .catch(err => {
      errorCb(err);
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

  return dispatch(requestAction(Endpoints.query, options as RequestInit)).then(
    () => {
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
    const functions = getState().tables.trackedFunctions;
    const remoteSchemas = getState().remoteSchemas.listData.remoteSchemas;
    const actions = getState().actions.common.actions;

    dispatch({
      type: 'Metadata/DROP_INCONSISTENT_METADATA_SUCCESS',
      data: inconsistentObjects,
    });

    if (inconsistentObjects.length > 0) {
      const filteredSchema = filterInconsistentMetadataObjects(
        allSchemas,
        inconsistentObjects,
        'tables'
      );
      const filteredFunctions = filterInconsistentMetadataObjects(
        functions,
        inconsistentObjects,
        'functions'
      );
      const filteredActions = filterInconsistentMetadataObjects(
        actions,
        inconsistentObjects,
        'actions'
      );

      // todo
      dispatch(setConsistentSchema(filteredSchema) as any);
      dispatch(setConsistentFunctions(filteredFunctions) as any);
      dispatch(setConsistentRemoteSchemas(remoteSchemas) as any);
      dispatch(setActions(filteredActions) as any);
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

    const { shouldReloadMetadata, shouldReloadRemoteSchemas } = reloadConfig;

    const loadQuery = shouldReloadMetadata
      ? getReloadCacheAndGetInconsistentObjectsQuery(
          !!shouldReloadRemoteSchemas
        )
      : inconsistentObjectsQuery;

    dispatch({ type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_REQUEST' });
    return dispatch(
      requestAction(Endpoints.query, {
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

    const reloadQuery = reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery(
      remoteSchemaName
    );

    dispatch({ type: 'Metadata/LOAD_INCONSISTENT_OBJECTS_REQUEST' });
    return dispatch(
      requestAction(Endpoints.query, {
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
      requestAction(Endpoints.query, {
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
    const headers = getState().tables.dataHeaders;

    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(updateAllowedQueryQuery(queryName, newQuery)),
      })
    ).then(
      () => {
        dispatch(showSuccessNotification('Updated allow-list query'));
        dispatch({
          type: 'Metadata/UPDATE_ALLOWED_QUERY',
          data: { queryName, newQuery },
        });
      },
      error => {
        console.error(error);
        dispatch(
          showErrorNotification('Updating allow-list query failed', null, error)
        );
      }
    );
  };
};

export const deleteAllowedQuery = (
  queryName: string,
  isLastQuery: boolean
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const headers = getState().tables.dataHeaders;

    const deleteQuery = isLastQuery
      ? deleteAllowListQuery()
      : deleteAllowedQueryQuery(queryName);

    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(deleteQuery),
      })
    ).then(
      () => {
        dispatch(showSuccessNotification('Deleted query from allow-list'));
        dispatch({ type: 'Metadata/DELETE_ALLOWED_QUERY', data: queryName });
      },
      error => {
        console.error(error);
        dispatch(
          showErrorNotification(
            'Deleting query from allow-list failed',
            null,
            error
          )
        );
      }
    );
  };
};

export const deleteAllowList = (): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const headers = getState().tables.dataHeaders;

    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(deleteAllowListQuery()),
      })
    ).then(
      () => {
        dispatch(
          showSuccessNotification('Deleted all queries from allow-list')
        );
        dispatch({ type: 'Metadata/DELETE_ALLOW_LIST' });
      },
      error => {
        console.error(error);
        dispatch(
          showErrorNotification(
            'Deleting queries from allow-list failed',
            null,
            error
          )
        );
      }
    );
  };
};

export const addAllowedQueries = (
  queries: Array<{ name: string; query: string }>,
  isEmptyList: boolean,
  callback: any
): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    if (queries.length === 0) {
      dispatch(showErrorNotification('No queries found', null));
      return;
    }
    const headers = getState().tables.dataHeaders;

    const addQuery = isEmptyList
      ? createAllowListQuery(queries)
      : addAllowedQueriesQuery(queries);

    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(addQuery),
      })
    ).then(
      () => {
        dispatch(
          showSuccessNotification(
            `${queries.length > 1 ? 'Queries' : 'Query'} added to allow-list`
          )
        );
        dispatch({ type: 'Metadata/ADD_ALLOWED_QUERIES', data: queries });
        callback();
      },
      error => {
        console.error(error);
        dispatch(
          showErrorNotification(
            'Adding query to allow-list failed',
            null,
            error
          )
        );
      }
    );
  };
};

export const loadAllowedQueries = (): Thunk<void, MetadataActions> => {
  return (dispatch, getState) => {
    const headers = getState().tables.dataHeaders;

    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(loadAllowedQueriesQuery),
      })
    ).then(
      data => {
        let queries;

        const collection = data[0];
        if (collection) {
          queries = collection.collection_defn.queries;
        } else {
          queries = [];
        }
        dispatch({ type: 'Metadata/LOAD_ALLOWED_QUERIES', data: queries });
      },
      error => {
        console.error(error);
        dispatch(showErrorNotification('Fetching allow list failed', null));
      }
    );
  };
};
