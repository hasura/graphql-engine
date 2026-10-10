import { GraphQLError } from 'graphql';
import {
  SET_ACTION_TIMEOUT,
  SET_ACTION_COMMENT,
  SET_ACTION_DEFINITION,
  SET_ACTION_HANDLER,
  SET_ACTION_KIND,
  SET_HEADERS,
  SET_TYPE_DEFINITION,
  TOGGLE_FORWARD_CLIENT_HEADERS,
  SetActionTimeout,
  SetActionExecution,
  SetActionComment,
  SetActionDefinition,
  SetTypeDefinition,
  SetHeaders,
  ToggleForwardClientHeaders,
  SetActionHandler,
  ActionFormEvents,
} from './types';
import { ActionExecution, ActionState } from '../../types';
import { ClientHeader } from '@hasura/shared/types';

export const setActionTimeout = (timeout: string): SetActionTimeout => ({
  type: SET_ACTION_TIMEOUT,
  timeout,
});

export const setActionHandler = (handler: string): SetActionHandler => ({
  type: SET_ACTION_HANDLER,
  handler,
});

export const setActionExecution = (
  kind: ActionExecution,
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
  error: GraphQLError | null | undefined = null,
  timer: NodeJS.Timeout | null | undefined,
  ast: Record<string, any> | null | undefined,
): SetActionDefinition => ({
  type: SET_ACTION_DEFINITION,
  definition: { sdl, error, timer, ast },
});

export const setTypeDefinition = (
  sdl: string,
  error: GraphQLError | null | undefined = null,
  timer: NodeJS.Timeout | null | undefined,
  ast: Record<string, any> | null | undefined,
): SetTypeDefinition => ({
  type: SET_TYPE_DEFINITION,
  definition: { sdl, error, timer, ast },
});

export const setHeaders = (headers: ClientHeader[]): SetHeaders => ({
  type: SET_HEADERS,
  headers,
});

export const toggleForwardClientHeaders = (): ToggleForwardClientHeaders => ({
  type: TOGGLE_FORWARD_CLIENT_HEADERS,
});

const reducer = (state: ActionState, action: ActionFormEvents): ActionState => {
  switch (action.type) {
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
    default:
      return state;
  }
};

export default reducer;
