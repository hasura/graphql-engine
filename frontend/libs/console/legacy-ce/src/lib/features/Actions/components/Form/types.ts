import { ClientHeader } from '@hasura/shared/types';
import { ActionExecution, Definition } from '../../types';

export const SET_ACTION_TIMEOUT = 'Actions/Add/SET_ACTION_TIMEOUT';
export const SET_ACTION_HANDLER = 'Actions/Add/SET_ACTION_HANDLER';
export const SET_ACTION_KIND = 'Actions/Add/SET_ACTION_KIND';
export const SET_ACTION_COMMENT = 'Actions/Add/SET_ACTION_COMMENT';
export const SET_ACTION_DEFINITION = 'Actions/Add/SET_ACTION_DEFINITION';
export const SET_TYPE_DEFINITION = 'Actions/Add/SET_TYPE_DEFINITION';
export const SET_HEADERS = 'Actions/Add/SET_HEADERS';
export const TOGGLE_FORWARD_CLIENT_HEADERS =
  'Actions/Add/TOGGLE_FORWARD_CLIENT_HEADERS';

export interface SetActionTimeout {
  type: typeof SET_ACTION_TIMEOUT;
  timeout: string;
}

export interface SetActionHandler {
  type: typeof SET_ACTION_HANDLER;
  handler: string;
}
export interface SetActionExecution {
  type: typeof SET_ACTION_KIND;
  kind: ActionExecution;
}

export interface SetActionComment {
  type: typeof SET_ACTION_COMMENT;
  comment: string;
}

export interface SetActionDefinition {
  type: typeof SET_ACTION_DEFINITION;
  definition: Definition;
}

export interface SetTypeDefinition {
  type: typeof SET_TYPE_DEFINITION;
  definition: Definition;
}
export interface SetHeaders {
  type: typeof SET_HEADERS;
  headers: ClientHeader[];
}

export interface ToggleForwardClientHeaders {
  type: typeof TOGGLE_FORWARD_CLIENT_HEADERS;
}

export type ActionFormEvents =
  | ToggleForwardClientHeaders
  | SetHeaders
  | SetTypeDefinition
  | SetActionDefinition
  | SetActionExecution
  | SetActionHandler
  | SetActionTimeout
  | SetActionComment;
