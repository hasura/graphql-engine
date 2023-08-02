import { Action as ReduxAction } from 'redux';
import { Header, Definition, ActionExecution } from '../Common/stateDefaults';

export const SET_DEFAULTS = 'Actions/Add/SET_DEFAULTS';
export const SET_ACTION_TIMEOUT = 'Actions/Add/SET_ACTION_TIMEOUT';
export const SET_ACTION_HANDLER = 'Actions/Add/SET_ACTION_HANDLER';
export const SET_ACTION_KIND = 'Actions/Add/SET_ACTION_KIND';
export const SET_ACTION_COMMENT = 'Actions/Add/SET_ACTION_COMMENT';
export const SET_ACTION_DEFINITION = 'Actions/Add/SET_ACTION_DEFINITION';
export const SET_TYPE_DEFINITION = 'Actions/Add/SET_TYPE_DEFINITION';
export const SET_DERIVED_ACTION_PARENT_OPERATION =
  'Actions/Add/SET_DERIVED_ACTION_PARENT_OPERATION';
export const RESET_DERIVED_ACTION_PARENT_OPERATION =
  'Actions/Add/RESET_DERIVED_ACTION_PARENT_OPERATION';
export const SET_FETCHING = 'Actions/Add/SET_FETCHING';
export const UNSET_FETCHING = 'Actions/Add/UNSET_FETCHING';
export const SET_HEADERS = 'Actions/Add/SET_HEADERS';
export const TOGGLE_FORWARD_CLIENT_HEADERS =
  'Actions/Add/TOGGLE_FORWARD_CLIENT_HEADERS';

export interface SetDefaults extends ReduxAction {
  type: typeof SET_DEFAULTS;
}

export interface SetActionTimeout extends ReduxAction {
  type: typeof SET_ACTION_TIMEOUT;
  timeout: string;
}

export interface SetActionHandler extends ReduxAction {
  type: typeof SET_ACTION_HANDLER;
  handler: string;
}
export interface SetActionExecution extends ReduxAction {
  type: typeof SET_ACTION_KIND;
  kind: ActionExecution;
}

export interface SetActionComment extends ReduxAction {
  type: typeof SET_ACTION_COMMENT;
  comment: string;
}

export interface SetActionDefinition extends ReduxAction {
  type: typeof SET_ACTION_DEFINITION;
  definition: Definition;
}

export interface SetTypeDefinition extends ReduxAction {
  type: typeof SET_TYPE_DEFINITION;
  definition: Definition;
}

export interface SetDerivedActionParentOperation extends ReduxAction {
  type: typeof SET_DERIVED_ACTION_PARENT_OPERATION;
  operationString: string;
}

export interface ResetDerivedActionParentOperation extends ReduxAction {
  type: typeof RESET_DERIVED_ACTION_PARENT_OPERATION;
}

export interface SetFetching extends ReduxAction {
  type: typeof SET_FETCHING;
}
export interface UnsetFetching extends ReduxAction {
  type: typeof UNSET_FETCHING;
}

export interface SetHeaders extends ReduxAction {
  type: typeof SET_HEADERS;
  headers: Header[];
}

export interface ToggleForwardClientHeaders extends ReduxAction {
  type: typeof TOGGLE_FORWARD_CLIENT_HEADERS;
}

export type AddActionEvents =
  | ToggleForwardClientHeaders
  | SetHeaders
  | UnsetFetching
  | SetFetching
  | ResetDerivedActionParentOperation
  | SetDerivedActionParentOperation
  | SetTypeDefinition
  | SetActionDefinition
  | SetActionExecution
  | SetActionHandler
  | SetActionTimeout
  | SetActionComment
  | SetDefaults;

export type DefaultState = {
  handler: string;
  actionDefinition: Definition;
  typeDefinition: Definition;
  headers: Header[];
  forwardClientHeaders: boolean;
  kind: 'synchronous' | 'asynchronous';
  isFetching: boolean;
  derive: { operation: string };
  timeout: string;
  comment: string;
};
