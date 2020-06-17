import { ThunkDispatch } from 'redux-thunk';
import { AnyAction } from 'redux';
import { push } from 'react-router-redux';
import requestAction from '../../../utils/requestAction';
import { clearIntrospectionSchemaCache } from '../RemoteSchema/graphqlUtils';
import globals from '../../../Globals';
import endpoints, { globalCookiePolicy } from '../../../Endpoints';
import defaultState from './State';
import { filterInconsistentMetadataObjects, MetadataObject } from './utils';
import {
  makeMigrationCall,
  setConsistentFunctions,
  setConsistentSchema,
} from '../Data/DataActions';
import { setConsistentRemoteSchemas } from '../RemoteSchema/Actions';
import { setActions } from '../Actions/reducer';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../Common/Notification';
import {
  dropInconsistentObjectsQuery,
  exportMetadataQuery,
  generateReplaceMetadataQuery,
  getReloadMetadataQuery,
  getReloadRemoteSchemaCacheQuery,
  inconsistentObjectsQuery,
  resetMetadataQuery,
} from '../../Common/utils/v1QueryUtils';
import { GetReduxState, ReduxState } from '../../../types';

const LOAD_INCONSISTENT_OBJECTS = 'Metadata/LOAD_INCONSISTENT_OBJECTS';
const LOADING_METADATA = 'Metadata/LOADING_METADATA';
const LOAD_METADATA_ERROR = 'Metadata/LOAD_METADATA_ERROR';
const DROP_INCONSISTENT_METADATA = 'Metadata/DROP_INCONSISTENT_METADATA';
const DROPPED_INCONSISTENT_METADATA = 'Metadata/DROPPED_INCONSISTENT_METADATA';
const DROPPING_INCONSISTENT_METADATA_FAILED =
  'Metadata/DROPPING_INCONSISTENT_METADATA_FAILED';

const LOAD_ALLOWED_QUERIES = 'Metadata/LOAD_ALLOWED_QUERIES';
const ADD_ALLOWED_QUERIES = 'Metadata/ADD_ALLOWED_QUERIES';
const UPDATE_ALLOWED_QUERY = 'Metadata/UPDATE_ALLOWED_QUERY';
const DELETE_ALLOWED_QUERY = 'Metadata/DELETE_ALLOWED_QUERY';
const DELETE_ALLOW_LIST = 'Metadata/DELETE_ALLOW_LIST';

const reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery = (
  remoteSchemaName: string
) => {
  return {
    type: 'bulk',
    args: [
      getReloadRemoteSchemaCacheQuery(remoteSchemaName),
      inconsistentObjectsQuery,
    ],
  };
};

const getReloadCacheAndGetInconsistentObjectsQuery = (
  shouldReloadRemoteSchemas: boolean
) => ({
  type: 'bulk',
  args: [
    getReloadMetadataQuery(shouldReloadRemoteSchemas),
    inconsistentObjectsQuery,
  ],
});

export const exportMetadata = (
  successCb: (oldMetadata: object) => void,
  errorCb: (e: Error) => void
) => (
  dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
  getState: GetReduxState
) => {
  const { dataHeaders } = getState().tables;

  const options: RequestInit = {
    method: 'POST',
    headers: {
      ...dataHeaders,
    },
    body: JSON.stringify(exportMetadataQuery),
  };

  dispatch(requestAction(endpoints.query, options))
    .then(response => {
      successCb(response);
    })
    .catch((err: Error) => {
      errorCb(err);
    });
};

export const replaceMetadata = (
  newMetadata: object,
  successCb: Function,
  errorCb: Function
) => (
  dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
  getState: GetReduxState
) => {
  const exportSuccessCb = (oldMetadata: object) => {
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
  errorCb: (e: Error) => void
) => (
  dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
  getState: GetReduxState
) => {
  const headers = getState().tables.dataHeaders;

  const options: RequestInit = {
    method: 'POST',
    credentials: globalCookiePolicy,
    headers: headers || {},
    body: JSON.stringify(resetMetadataQuery),
  };

  return dispatch(requestAction(endpoints.query, options)).then(
    () => {
      if (successCb) {
        successCb();
      }
      dispatch(showSuccessNotification('Metadata reset successfully!'));
    },
    (error: Error) => {
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
  errorCb: (err?: Error) => void
) => (dispatch: ThunkDispatch<ReduxState, {}, AnyAction>) => {
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

const handleInconsistentObjects = (inconsistentObjects: MetadataObject[]) => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const allSchemas = getState().tables.allSchemas;
    const functions = getState().tables.trackedFunctions;
    const remoteSchemas = getState().remoteSchemas.listData.remoteSchemas;
    const actions = getState().actions.common.actions;

    dispatch({
      type: LOAD_INCONSISTENT_OBJECTS,
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
      const filteredRemoteSchemas = filterInconsistentMetadataObjects(
        remoteSchemas,
        inconsistentObjects,
        'remote_schemas'
      );
      const filteredActions = filterInconsistentMetadataObjects(
        actions,
        inconsistentObjects,
        'actions'
      );

      dispatch(setConsistentSchema(filteredSchema));
      dispatch(setConsistentFunctions(filteredFunctions));
      dispatch(setConsistentRemoteSchemas(filteredRemoteSchemas));
      dispatch(setActions(filteredActions));
    }
  };
};

export const loadInconsistentObjects = (
  reloadConfig: ReloadConfig,
  successCb?: Function,
  failureCb?: Function
) => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const headers = getState().tables.dataHeaders;

    const { shouldReloadMetadata, shouldReloadRemoteSchemas } = reloadConfig;

    const loadQuery = shouldReloadMetadata
      ? getReloadCacheAndGetInconsistentObjectsQuery(
          shouldReloadRemoteSchemas !== false
        )
      : inconsistentObjectsQuery;

    dispatch({ type: LOADING_METADATA });
    return dispatch(
      requestAction(endpoints.query, {
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
      (error: Error) => {
        console.error(error);
        dispatch({ type: LOAD_METADATA_ERROR });
        if (failureCb) {
          failureCb(error);
        }
      }
    );
  };
};

/* Reloads only remote schema metadata */

export const reloadRemoteSchema = (
  remoteSchemaName: string,
  successCb?: Function,
  failureCb?: Function
) => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const headers = getState().tables.dataHeaders;

    const reloadQuery = reloadRemoteSchemaCacheAndGetInconsistentObjectsQuery(
      remoteSchemaName
    );

    dispatch({ type: LOADING_METADATA });
    return dispatch(
      requestAction(endpoints.query, {
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
      (error: Error) => {
        console.error(error);
        dispatch({ type: LOAD_METADATA_ERROR });
        if (failureCb) {
          failureCb(error);
        }
      }
    );
  };
};

export const reloadMetadata = (
  shouldReloadRemoteSchemas: boolean,
  successCb?: Function,
  failureCb?: Function
) => {
  return (dispatch: ThunkDispatch<ReduxState, {}, AnyAction>) => {
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
  successCb: Function,
  failureCb: Function
) => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const headers = getState().tables.dataHeaders;
    dispatch({ type: DROP_INCONSISTENT_METADATA });
    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(dropInconsistentObjectsQuery),
      })
    ).then(
      () => {
        dispatch({ type: DROPPED_INCONSISTENT_METADATA });
        dispatch(showSuccessNotification('Dropped inconsistent metadata'));
        dispatch(loadInconsistentObjects({ shouldReloadRemoteSchemas: false }));
        clearIntrospectionSchemaCache();
        if (successCb) {
          successCb();
        }
      },
      (error: Error) => {
        console.error(error);
        dispatch({ type: DROPPING_INCONSISTENT_METADATA_FAILED });
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

export const isMetadataStatusPage = () => {
  return window.location.pathname.includes('/settings/metadata-status');
};

export const redirectToMetadataStatus = () => {
  return (dispatch: ThunkDispatch<ReduxState, {}, AnyAction>) => {
    return dispatch(
      push(`${globals.urlPrefix}/settings/metadata-status?is_redirected=true`)
    );
  };
};

const allowedQueriesCollection = 'allowed-queries';

const loadAllowedQueriesQuery = () => ({
  type: 'select',
  args: {
    table: {
      name: 'hdb_query_collection',
      schema: 'hdb_catalog',
    },
    columns: ['collection_defn'],
    where: { collection_name: allowedQueriesCollection },
  },
});

const createAllowListQuery = (queries: Query[]) => {
  const createAllowListCollectionQuery = () => ({
    type: 'create_query_collection',
    args: {
      name: allowedQueriesCollection,
      definition: {
        queries,
      },
    },
  });

  const addCollectionToAllowListQuery = () => ({
    type: 'add_collection_to_allowlist',
    args: {
      collection: allowedQueriesCollection,
    },
  });

  return {
    type: 'bulk',
    args: [createAllowListCollectionQuery(), addCollectionToAllowListQuery()],
  };
};

const deleteAllowListQuery = () => ({
  type: 'drop_query_collection',
  args: {
    collection: allowedQueriesCollection,
    cascade: true,
  },
});

const addAllowedQueryQuery = (query: Query) => ({
  type: 'add_query_to_collection',
  args: {
    collection_name: allowedQueriesCollection,
    query_name: query.name,
    query: query.query,
  },
});

const addAllowedQueriesQuery = (queries: Query[]) => {
  const addQueries = queries.map(query => addAllowedQueryQuery(query));

  return {
    type: 'bulk',
    args: addQueries,
  };
};

const deleteAllowedQueryQuery = (queryName: string) => ({
  type: 'drop_query_from_collection',
  args: {
    collection_name: allowedQueriesCollection,
    query_name: queryName,
  },
});

const updateAllowedQueryQuery = (queryName: string, newQuery: Query) => ({
  type: 'bulk',
  args: [deleteAllowedQueryQuery(queryName), addAllowedQueryQuery(newQuery)],
});

export const loadAllowedQueries = () => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const headers = getState().tables.dataHeaders;

    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(loadAllowedQueriesQuery()),
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
        dispatch({ type: LOAD_ALLOWED_QUERIES, data: queries });
      },
      (error: Error) => {
        console.error(error);
        dispatch(showErrorNotification('Fetching allow list failed'));
      }
    );
  };
};

export const addAllowedQueries = (
  queries: Query[],
  isEmptyList: boolean,
  callback: Function
): Function => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ): void | Promise<void> => {
    if (queries.length === 0) {
      return dispatch(showErrorNotification('No queries found'));
    }

    const headers = getState().tables.dataHeaders;

    const addQuery = isEmptyList
      ? createAllowListQuery(queries)
      : addAllowedQueriesQuery(queries);

    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(addQuery),
      })
    ).then(
      (): void => {
        dispatch(
          showSuccessNotification(
            `${queries.length > 1 ? 'Queries' : 'Query'} added to allow-list`
          )
        );
        dispatch({ type: ADD_ALLOWED_QUERIES, data: queries });
        callback();
      },
      (error: Error): void => {
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

export const deleteAllowList = () => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const headers = getState().tables.dataHeaders;

    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(deleteAllowListQuery()),
      })
    ).then(
      () => {
        dispatch(
          showSuccessNotification('Deleted all queries from allow-list')
        );
        dispatch({ type: DELETE_ALLOW_LIST });
      },
      (error: Error) => {
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

export const deleteAllowedQuery = (queryName: string, isLastQuery: boolean) => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const headers = getState().tables.dataHeaders;

    const deleteQuery = isLastQuery
      ? deleteAllowListQuery()
      : deleteAllowedQueryQuery(queryName);

    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(deleteQuery),
      })
    ).then(
      () => {
        dispatch(showSuccessNotification('Deleted query from allow-list'));
        dispatch({ type: DELETE_ALLOWED_QUERY, data: queryName });
      },
      (error: Error) => {
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

export const updateAllowedQuery = (queryName: string, newQuery: Query) => {
  return (
    dispatch: ThunkDispatch<ReduxState, {}, AnyAction>,
    getState: GetReduxState
  ) => {
    const headers = getState().tables.dataHeaders;

    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(updateAllowedQueryQuery(queryName, newQuery)),
      })
    ).then(
      () => {
        dispatch(showSuccessNotification('Updated allow-list query'));
        dispatch({ type: UPDATE_ALLOWED_QUERY, data: { queryName, newQuery } });
      },
      (error: Error) => {
        console.error(error);
        dispatch(
          showErrorNotification('Updating allow-list query failed', null, error)
        );
      }
    );
  };
};

export const metadataReducer = (
  state: SettingsState = defaultState,
  action: MetadataReducerAction
) => {
  switch (action.type) {
    case LOAD_INCONSISTENT_OBJECTS:
      return {
        ...state,
        inconsistentObjects: action.data,
        ongoingRequest: false,
      };
    case LOAD_METADATA_ERROR:
      return {
        ...state,
        error: true,
        ongoingRequest: false,
      };
    case LOADING_METADATA:
      return {
        ...state,
        ongoingRequest: true,
      };
    case DROP_INCONSISTENT_METADATA:
      return {
        ...state,
        ongoingRequest: true,
      };
    case DROPPED_INCONSISTENT_METADATA:
      return {
        ...state,
        inconsistentObjects: [],
        ongoingRequest: false,
      };
    case DROPPING_INCONSISTENT_METADATA_FAILED:
      return {
        ...state,
        ongoingRequest: false,
      };
    case LOAD_ALLOWED_QUERIES:
      return {
        ...state,
        allowedQueries: action.data,
      };
    case ADD_ALLOWED_QUERIES:
      return {
        ...state,
        allowedQueries: [...state.allowedQueries, ...(action.data as Query[])],
      };
    case DELETE_ALLOW_LIST:
      return {
        ...state,
        allowedQueries: [],
      };
    case DELETE_ALLOWED_QUERY:
      return {
        ...state,
        allowedQueries: [
          ...state.allowedQueries.filter((q: Query) => q.name !== action.data),
        ],
      };
    case UPDATE_ALLOWED_QUERY:
      return {
        ...state,
        allowedQueries: [
          ...state.allowedQueries.map((q: Query) => {
            const { queryName, newQuery } = action.data as UpdateAllowedQuery;
            return q.name === queryName ? newQuery : q;
          }),
        ],
      };
    default:
      return state;
  }
};

export type SettingsState = {
  inconsistentObjects: object[];
  ongoingRequest: boolean;
  allowedQueries: Query[];
};

type ReloadConfig = {
  shouldReloadMetadata?: boolean;
  shouldReloadRemoteSchemas?: boolean;
};

type Query = { name: string; query: string };

type UpdateAllowedQuery = { queryName: string; newQuery: Query };

type MetadataReducerAction = {
  type:
    | typeof LOAD_INCONSISTENT_OBJECTS
    | typeof LOADING_METADATA
    | typeof LOAD_METADATA_ERROR
    | typeof DROP_INCONSISTENT_METADATA
    | typeof DROPPED_INCONSISTENT_METADATA
    | typeof DROPPING_INCONSISTENT_METADATA_FAILED
    | typeof LOAD_ALLOWED_QUERIES
    | typeof ADD_ALLOWED_QUERIES
    | typeof UPDATE_ALLOWED_QUERY
    | typeof DELETE_ALLOWED_QUERY
    | typeof DELETE_ALLOW_LIST;
  data: Array<Query> | Query | string | Array<object> | UpdateAllowedQuery;
};
