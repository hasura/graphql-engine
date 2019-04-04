import requestAction from '../../../../utils/requestAction';
import { push } from 'react-router-redux';
import globals from '../../../../Globals';
import endpoints from '../../../../Endpoints';
import defaultState from './State';
import semverCheck from '../../../../helpers/semver';
import { filterSchema } from './metadataFilters';
import { setConsistentSchema, setConsistentFunctions } from '../DataActions';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../Notification';

const LOAD_INCONSISTENT_OBJECTS = 'Metadata/LOAD_INCONSISTENT_OBJECTS';
const LOADING_METADATA = 'Metadata/LOADING_METADATA';
const LOAD_METADATA_ERROR = 'Metadata/LOAD_METADATA_ERROR';
const DROP_INCONSISTENT_METADATA = 'Metadata/DROP_INCONSISTENT_METADATA';
const DROPPED_INCONSISTENT_METADATA = 'Metadata/DROPPED_INCONSISTENT_METADATA';
const DROPPING_INCONSISTENT_METADATA_FAILED =
  'Metadata/DROPPING_INCONSISTENT_METADATA_FAILED';

const getInconsistentObjectsQuery = {
  type: 'get_inconsistent_metadata',
  args: {},
};

const reloadCacheAndGetInconsistentObjectsQuery = {
  type: 'bulk',
  args: [
    {
      type: 'reload_metadata',
      args: {},
    },
    getInconsistentObjectsQuery,
  ],
};

const dropInconsistentObjectsQuery = {
  type: 'drop_inconsistent_metadata',
  args: {},
};

export const filterInconsistentMetadata = (
  metadata,
  inconsistentObjects,
  type
) => {
  let filtered = JSON.parse(JSON.stringify(metadata));
  inconsistentObjects.forEach(object => {
    const partiallyFiltered = filterSchema(filtered, object, type);
    filtered = partiallyFiltered;
  });
  return filtered;
};

export const loadInconsistentObjects = (serverVersion, shouldReloadCache) => {
  return (dispatch, getState) => {
    if (!semverCheck('inconsistentState', serverVersion)) {
      return;
    }
    if (!serverVersion) {
      const serverVersionFromState = getState().main.serverVersion;
      if (!semverCheck('inconsistentState', serverVersionFromState)) {
        return;
      }
    }
    const headers = getState().tables.dataHEaders;
    dispatch({ type: LOADING_METADATA });
    return dispatch(
      requestAction(endpoints.query, {
        method: 'POST',
        headers,
        body: JSON.stringify(
          shouldReloadCache
            ? reloadCacheAndGetInconsistentObjectsQuery
            : getInconsistentObjectsQuery
        ),
      })
    ).then(
      data => {
        const allSchemas = getState().tables.allSchemas;
        const functions = getState().tables.trackedFunctions;
        const inconsistentObjects = shouldReloadCache ? data[1] : data;
        dispatch({
          type: LOAD_INCONSISTENT_OBJECTS,
          data: inconsistentObjects,
        });
        if (inconsistentObjects.length > 0) {
          const filteredSchema = filterInconsistentMetadata(
            allSchemas,
            inconsistentObjects,
            'tables'
          );
          const filteredFunctions = filterInconsistentMetadata(
            functions,
            inconsistentObjects,
            'functions'
          );
          dispatch(setConsistentSchema(filteredSchema));
          dispatch(setConsistentFunctions(filteredFunctions));
        }
      },
      error => {
        console.error(error);
        dispatch({ type: LOAD_METADATA_ERROR });
      }
    );
  };
};

export const dropInconsistentObjects = () => {
  return (dispatch, getState) => {
    const headers = getState().tables.dataHEaders;
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
        dispatch(loadInconsistentObjects(null, false));
      },
      error => {
        console.error(error);
        dispatch({ type: DROPPING_INCONSISTENT_METADATA_FAILED });
        dispatch(
          showErrorNotification('Dropping inconsistent metadata failed')
        );
      }
    );
  };
};

export const redirectToMetadataStatus = () => {
  return dispatch => {
    return dispatch(
      push(globals.urlPrefix + '/metadata/status?is_redirected=true')
    );
  };
};

export const metadataReducer = (state = defaultState, action) => {
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
    default:
      return state;
  }
};
