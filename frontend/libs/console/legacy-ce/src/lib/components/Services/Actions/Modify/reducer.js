import defaultState from './state';

const SET_MODIFY_STATE = 'Actions/Modify/SET_MODIFY_STATE';
export const setModifyState = state => ({
  type: SET_MODIFY_STATE,
  state,
});

const SET_ACTION_TIMEOUT = 'Actions/Add/SET_ACTION_TIMEOUT';
export const setActionTimeout = timeout => ({
  type: SET_ACTION_TIMEOUT,
  timeout,
});

const SET_ACTION_HANDLER = 'Actions/Modify/SET_ACTION_HANDLER';
export const setActionHandler = handler => ({
  type: SET_ACTION_HANDLER,
  handler,
});

const SET_ACTION_KIND = 'Actions/Modify/SET_ACTION_KIND';
export const setActionKind = kind => ({
  type: SET_ACTION_KIND,
  kind,
});

const SET_ACTION_COMMENT = 'Actions/Modify/SET_ACTION_COMMENT';
export const setActionComment = comment => ({
  type: SET_ACTION_COMMENT,
  comment,
});

const SET_ACTION_DEFINITION = 'Actions/Modify/SET_ACTION_DEFINITION';
export const setActionDefinition = (sdl, error, timer, ast) => ({
  type: SET_ACTION_DEFINITION,
  definition: { sdl, error, timer, ast },
});

const SET_TYPE_DEFINITION = 'Actions/Modify/SET_TYPE_DEFINITION';
export const setTypeDefinition = (sdl, error = null, timer, ast) => ({
  type: SET_TYPE_DEFINITION,
  definition: { sdl, error, timer, ast },
});

const SET_FETCHING = 'Actions/Modify/SET_FETCHING';
export const setFetching = () => ({ type: SET_FETCHING });
const UNSET_FETCHING = 'Actions/Modify/UNSET_FETCHING';
export const unsetFetching = () => ({ type: UNSET_FETCHING });

const SET_HEADERS = 'Actions/Modify/SET_HEADERS';
export const setHeaders = headers => ({
  type: SET_HEADERS,
  headers,
});

const TOGGLE_FORWARD_CLIENT_HEADERS =
  'Actions/Modify/TOGGLE_FORWARD_CLIENT_HEADERS';
export const toggleForwardClientHeaders = () => ({
  type: TOGGLE_FORWARD_CLIENT_HEADERS,
});

const reducer = (state = defaultState, action) => {
  switch (action.type) {
    case SET_MODIFY_STATE:
      return {
        ...action.state,
      };
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
    default:
      return state;
  }
};

export default reducer;
