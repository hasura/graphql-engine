import { Action } from 'redux';

/* Default state */
const defaultHeader = {
  name: '',
  type: 'static',
  value: '',
};
const defaultState = {
  headers: [defaultHeader],
};
export const HEADER_KEY_CHANGE = 'HEADER/HEADER_KEY_CHANGE';
export const HEADER_VALUE_TYPE_CHANGE = 'HEADER/HEADER_VALUE_TYPE_CHANGE';
export const HEADER_VALUE_CHANGE = 'HEADER/HEADER_VALUE_CHANGE';
export const UPDATE_HEADERS = 'HEADER/UPDATE_HEADERS';
export const RESET_HEADER = 'HEADER/RESET_HEADER';
export const ADD_NEW_HEADER = 'HEADER/ADD_NEW_HEADER';
export const DELETE_HEADER = 'HEADER/DELETE_HEADER';

/* Types */

export type IHeader = typeof defaultHeader;
export type IState = typeof defaultState;

interface HeaderKeyChange extends Action {
  type: typeof HEADER_KEY_CHANGE;
  data: { index: number; name: string };
}
interface HeaderValueTypeChange extends Action {
  type: typeof HEADER_VALUE_TYPE_CHANGE;
  data: { index: number; type: string };
}
interface HeaderValueChange extends Action {
  type: typeof HEADER_VALUE_CHANGE;
  data: { index: number; value: string };
}
interface AddNewHeader extends Action {
  type: typeof ADD_NEW_HEADER;
}
interface DeleteHeader extends Action {
  type: typeof DELETE_HEADER;
  data: { index: number; name: string };
}
interface ResetHeader extends Action {
  type: typeof RESET_HEADER;
}
interface UpdateHeaders extends Action {
  type: typeof UPDATE_HEADERS;
  data: IHeader[];
}

export type HeaderEvents =
  | HeaderKeyChange
  | HeaderValueTypeChange
  | HeaderValueChange
  | AddNewHeader
  | DeleteHeader
  | ResetHeader
  | UpdateHeaders;

/* Reducer */
const headerReducer = (state = defaultState, action: HeaderEvents) => {
  switch (action.type) {
    case HEADER_KEY_CHANGE:
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, action.data.index),
          {
            ...state.headers[action.data.index],
            name: action.data.name,
          },
          ...state.headers.slice(action.data.index + 1, state.headers.length),
        ],
      };
    case HEADER_VALUE_TYPE_CHANGE:
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, action.data.index),
          {
            ...state.headers[action.data.index],
            type: action.data.type,
          },
          ...state.headers.slice(action.data.index + 1, state.headers.length),
        ],
      };
    case HEADER_VALUE_CHANGE:
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, action.data.index),
          {
            ...state.headers[action.data.index],
            value: action.data.value,
          },
          ...state.headers.slice(action.data.index + 1, state.headers.length),
        ],
      };
    case ADD_NEW_HEADER:
      return {
        ...state,
        headers: [...state.headers, { ...defaultState.headers[0] }],
      };

    case DELETE_HEADER:
      return {
        ...state,
        headers: [
          ...state.headers.slice(0, action.data.index),
          ...state.headers.slice(action.data.index + 1, state.headers.length),
        ],
      };
    case RESET_HEADER:
      return {
        ...defaultState,
      };
    case UPDATE_HEADERS:
      return {
        ...state,
        headers: [...action.data],
      };
    default:
      return {
        ...state,
      };
  }
};

export default headerReducer;
