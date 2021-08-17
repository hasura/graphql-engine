import defaultState from './state';

const SET_DEFAULTS = 'Actions/Add/SET_DEFAULTS';
export const setDefaults = () => ({ type: SET_DEFAULTS });

const SET_ACTION_TIMEOUT = 'Actions/Add/SET_ACTION_TIMEOUT';
export const setActionTimeout = timeout => ({
  type: SET_ACTION_TIMEOUT,
  timeout,
});

const SET_ACTION_HANDLER = 'Actions/Add/SET_ACTION_HANDLER';
export const setActionHandler = handler => ({
  type: SET_ACTION_HANDLER,
  handler,
});

const SET_ACTION_KIND = 'Actions/Add/SET_ACTION_KIND';
export const setActionKind = kind => ({
  type: SET_ACTION_KIND,
  kind,
});

const SET_ACTION_COMMENT = 'Actions/Add/SET_ACTION_COMMENT';
export const setActionComment = comment => ({
  type: SET_ACTION_COMMENT,
  comment,
});

const SET_ACTION_DEFINITION = 'Actions/Add/SET_ACTION_DEFINITION';
export const setActionDefinition = (sdl, error = null, timer, ast) => ({
  type: SET_ACTION_DEFINITION,
  definition: { sdl, error, timer, ast },
});

const SET_TYPE_DEFINITION = 'Actions/Add/SET_TYPE_DEFINITION';
export const setTypeDefinition = (sdl, error = null, timer, ast) => ({
  type: SET_TYPE_DEFINITION,
  definition: { sdl, error, timer, ast },
});

const SET_DERIVED_ACTION_PARENT_OPERATION =
  'Actions/Add/SET_DERIVED_ACTION_PARENT_OPERATION';
export const setDerivedActionParentOperation = operationString => ({
  type: SET_DERIVED_ACTION_PARENT_OPERATION,
  data: operationString,
});

const RESET_DERIVED_ACTION_PARENT_OPERATION =
  'Actions/Add/RESET_DERIVED_ACTION_PARENT_OPERATION';
export const resetDerivedActionParentOperation = () => ({
  type: RESET_DERIVED_ACTION_PARENT_OPERATION,
});

const SET_FETCHING = 'Actions/Add/SET_FETCHING';
export const setFetching = () => ({ type: SET_FETCHING });
const UNSET_FETCHING = 'Actions/Add/UNSET_FETCHING';
export const unsetFetching = () => ({ type: UNSET_FETCHING });

const SET_HEADERS = 'Actions/Add/SET_HEADERS';
export const setHeaders = headers => ({
  type: SET_HEADERS,
  headers,
});

const TOGGLE_FORWARD_CLIENT_HEADERS =
  'Actions/Add/TOGGLE_FORWARD_CLIENT_HEADERS';
export const toggleForwardClientHeaders = () => ({
  type: TOGGLE_FORWARD_CLIENT_HEADERS,
});

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_DEFAULTS:
      return defaultState;
    case SET_ACTION_TIMEOUT:
      return {
        ...state,
        timeout: action.timeout,
      };
    case SET_ACTION_HANDLER:
      return {
        ...state,
        handler: action.handler,
      };
    case SET_ACTION_KIND:
      return {
        ...state,
        kind: action.kind,
      };
    case SET_ACTION_COMMENT:
      return {
        ...state,
        comment: action.comment,
      };
    case SET_FETCHING:
      return {
        ...state,
        isFetching: true,
      };
    case UNSET_FETCHING:
      return {
        ...state,
        isFetching: false,
      };
    case SET_ACTION_DEFINITION:
      if (action.definition) {
        return {
          ...state,
          actionDefinition: {
            ...action.definition,
            sdl:
              action.definition.sdl !== null
                ? action.definition.sdl
                : state.actionDefinition.sdl,
          },
        };
      }
      return state;
    case SET_TYPE_DEFINITION:
      return {
        ...state,
        typeDefinition: {
          ...action.definition,
          sdl:
            action.definition.sdl !== null
              ? action.definition.sdl
              : state.typeDefinition.sdl,
        },
      };
    case SET_HEADERS:
      return {
        ...state,
        headers: action.headers,
      };
    case TOGGLE_FORWARD_CLIENT_HEADERS:
      return {
        ...state,
        forwardClientHeaders: !state.forwardClientHeaders,
      };
    case SET_DERIVED_ACTION_PARENT_OPERATION:
      return {
        ...state,
        derive: {
          operation: action.data,
        },
      };
    case RESET_DERIVED_ACTION_PARENT_OPERATION:
      return {
        ...state,
        derive: {
          operation: '',
        },
      };
    default:
      return state;
  }
};

export default reducer;
