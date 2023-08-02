import { GraphQLError } from 'graphql';
import {
  SET_ACTION_TIMEOUT,
  SET_ACTION_COMMENT,
  SET_ACTION_DEFINITION,
  SET_ACTION_HANDLER,
  SET_ACTION_KIND,
  SET_DEFAULTS,
  SET_DERIVED_ACTION_PARENT_OPERATION,
  SET_FETCHING,
  SET_HEADERS,
  SET_TYPE_DEFINITION,
  TOGGLE_FORWARD_CLIENT_HEADERS,
  RESET_DERIVED_ACTION_PARENT_OPERATION,
  UNSET_FETCHING,
  SetDefaults,
  SetActionTimeout,
  SetActionExecution,
  SetActionComment,
  SetActionDefinition,
  SetTypeDefinition,
  SetDerivedActionParentOperation,
  ResetDerivedActionParentOperation,
  SetFetching,
  UnsetFetching,
  SetHeaders,
  ToggleForwardClientHeaders,
  SetActionHandler,
  AddActionEvents,
  DefaultState,
} from './types';
import defaultState from './state';
import { ActionExecution, Header } from '../Common/stateDefaults';
import { Nullable } from '../../../Common/utils/tsUtils';

export const setDefaults = (): SetDefaults => ({ type: SET_DEFAULTS });

export const setActionTimeout = (timeout: string): SetActionTimeout => ({
  type: SET_ACTION_TIMEOUT,
  timeout,
});

export const setActionHandler = (handler: string): SetActionHandler => ({
  type: SET_ACTION_HANDLER,
  handler,
});

export const setActionExecution = (
  kind: ActionExecution
): SetActionExecution => ({
  type: SET_ACTION_KIND,
  kind,
});

export const setActionComment = (comment: string): SetActionComment => ({
  type: SET_ACTION_COMMENT,
  comment,
});

export const setActionDefinition = (
  sdl: string,
  error: Nullable<GraphQLError> = null,
  timer: Nullable<NodeJS.Timeout>,
  ast: Nullable<Record<string, any>>
): SetActionDefinition => ({
  type: SET_ACTION_DEFINITION,
  definition: { sdl, error, timer, ast },
});

export const setTypeDefinition = (
  sdl: string,
  error: Nullable<GraphQLError> = null,
  timer: Nullable<NodeJS.Timeout>,
  ast: Nullable<Record<string, any>>
): SetTypeDefinition => ({
  type: SET_TYPE_DEFINITION,
  definition: { sdl, error, timer, ast },
});

export const setDerivedActionParentOperation = (
  operationString: string
): SetDerivedActionParentOperation => ({
  type: SET_DERIVED_ACTION_PARENT_OPERATION,
  operationString,
});

export const resetDerivedActionParentOperation =
  (): ResetDerivedActionParentOperation => ({
    type: RESET_DERIVED_ACTION_PARENT_OPERATION,
  });

export const setFetching = (): SetFetching => ({ type: SET_FETCHING });
export const unsetFetching = (): UnsetFetching => ({ type: UNSET_FETCHING });

export const setHeaders = (headers: Header[]): SetHeaders => ({
  type: SET_HEADERS,
  headers,
});

export const toggleForwardClientHeaders = (): ToggleForwardClientHeaders => ({
  type: TOGGLE_FORWARD_CLIENT_HEADERS,
});

const reducer = (
  state = defaultState,
  action: AddActionEvents
): DefaultState => {
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
          operation: action.operationString,
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
