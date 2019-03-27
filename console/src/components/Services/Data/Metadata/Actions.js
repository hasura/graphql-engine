import requestAction from '../../../../utils/requestAction';
import endpoints from '../../../../Endpoints';
import defaultState from './State';
import semverCheck from '../../../../helpers/semver';

const LOAD_INCONSISTENT_OBJECTS = 'Metadata/LOAD_INCONSISTENT_OBJECTS';
const LOADING_INCONSISTENT_OBJECTS = 'Metadata/LOADING_INCONSISTENT_OBJECTS';
const LOAD_INCONSISTENT_OBJECTS_ERROR =
  'Metadata/LOAD_INCONSISTENT_OBJECTS_ERROR';

const getInconsistentObjectsQuery = {
  type: 'bulk',
  args: [
    {
      type: 'reload_metadata',
      args: {},
    },
    {
      type: 'get_inconsistent_objects',
      args: {},
    },
  ],
};

export const loadInconsistentObjects = serverVersion => {
  return (dispatch, getState) => {
    if (!semverCheck('inconsistentState', serverVersion)) {
      return;
    }
    const headers = getState().tables.dataHEaders;
    dispatch({ type: LOADING_INCONSISTENT_OBJECTS });
    return dispatch(
      requestAction(
        endpoints.query,
        {
          method: 'POST',
          headers,
          body: JSON.stringify(getInconsistentObjectsQuery),
        },
        LOAD_INCONSISTENT_OBJECTS,
        LOAD_INCONSISTENT_OBJECTS_ERROR
      )
    );
  };
};

export const metadataReducer = (state = defaultState, action) => {
  switch (action.type) {
    case LOAD_INCONSISTENT_OBJECTS:
      return {
        ...state,
        inconsistentObjects: action.data[1],
        ongoingRequest: false,
      };
    case LOAD_INCONSISTENT_OBJECTS_ERROR:
      return {
        ...state,
        error: true,
        ongoingRequest: false,
      };
    case LOADING_INCONSISTENT_OBJECTS:
      return {
        ...state,
        ongoingRequest: true,
      };
    default:
      return state;
  }
};
