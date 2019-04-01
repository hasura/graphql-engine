import requestAction from '../../../../utils/requestAction';
import endpoints from '../../../../Endpoints';
import defaultState from './State';
import semverCheck from '../../../../helpers/semver';
import { filterSchema } from './schemaFilters';
import { setFilteredSchema } from '../DataActions';

const RELOAD_CACHE_AND_LOAD_INCONSISTENT_OBJECTS =
  'Metadata/RELOAD_CACHE_AND_LOAD_INCONSISTENT_OBJECTS';
const LOAD_INCONSISTENT_OBJECTS = 'Metadata/LOAD_INCONSISTENT_OBJECTS';
const LOADING_METADATA = 'Metadata/LOADING_METADATA';
const LOAD_METADATA_ERROR = 'Metadata/LOAD_METADATA_ERROR';

const getInconsistentObjectsQuery = {
  type: 'get_inconsistent_objects',
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

export const filterInconsistentSchema = (allSchemas, inconsistentObjects) => {
  let filteredSchema = JSON.parse(JSON.stringify(allSchemas));
  inconsistentObjects.forEach(object => {
    const partiallyFiltered = filterSchema(filteredSchema, object);
    filteredSchema = partiallyFiltered;
  });
  return filteredSchema;
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
        const inconsistentObjects = shouldReloadCache ? data[1] : data;
        dispatch({
          type: LOAD_INCONSISTENT_OBJECTS,
          data: inconsistentObjects,
        });
        if (inconsistentObjects.length > 0) {
          const filteredSchema = filterInconsistentSchema(
            allSchemas,
            inconsistentObjects
          );
          dispatch(setFilteredSchema(filteredSchema));
        }
      },
      error => {
        console.error(error);
        dispatch({ type: LOAD_METADATA_ERROR });
      }
    );
  };
};

export const metadataReducer = (state = defaultState, action) => {
  switch (action.type) {
    case RELOAD_CACHE_AND_LOAD_INCONSISTENT_OBJECTS:
      return {
        ...state,
        inconsistentObjects: action.data[1],
        ongoingRequest: false,
      };
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
    default:
      return state;
  }
};
