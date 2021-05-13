import {
  defaultHeaderState,
  defaultLoadingState,
  defaultVariableState,
  HeaderState,
  VariableState,
} from './state';

export interface SetHeaderKeyText {
  type: 'RequestHeaders/SET_HEADER_KEY_TEXT';
  data: string;
  index: number;
}

export interface SetHeaderValueText {
  type: 'RequestHeaders/SET_HEADER_VALUE_TEXT';
  data: string;
  index: number;
}

export interface RemoveHeaderFromList {
  type: 'RequestHeaders/REMOVE_HEADER_FROM_LIST';
  index: number;
}

export interface ToggleActiveStateOfHeader {
  type: 'RequestHeaders/TOGGLE_ACTIVE_STATE_OF_HEADER';
  data: boolean;
  index: number;
}

export interface SetHeaderStateToDefault {
  type: 'RequestHeaders/SET_HEADERS_TO_DEFAULT_STATE';
}

export interface AddNewEmptyHeader {
  type: 'RequestHeaders/ADD_NEW_EMPTY_HEADER';
}

const getEmptyHeader = (headerState: HeaderState[]) => {
  const newIndex = headerState?.length ?? 1;

  return {
    key: '',
    value: '',
    isActive: true,
    index: newIndex,
  };
};

const updatePropertyOnHeaderState = (
  header: HeaderState,
  property: keyof HeaderState,
  value: string | boolean,
  updateIndex: number
) => {
  if (header.index !== updateIndex) {
    return header;
  }
  return { ...header, [property]: value };
};

export type HeaderStateAction =
  | SetHeaderKeyText
  | SetHeaderValueText
  | RemoveHeaderFromList
  | SetHeaderStateToDefault
  | ToggleActiveStateOfHeader
  | AddNewEmptyHeader;

export const headerStateReducer = (
  state = defaultHeaderState,
  action: HeaderStateAction
): HeaderState[] => {
  switch (action.type) {
    case 'RequestHeaders/SET_HEADER_KEY_TEXT':
      return state.map(header =>
        updatePropertyOnHeaderState(header, 'key', action.data, action.index)
      );
    case 'RequestHeaders/SET_HEADER_VALUE_TEXT':
      return state.map(header =>
        updatePropertyOnHeaderState(header, 'value', action.data, action.index)
      );
    case 'RequestHeaders/TOGGLE_ACTIVE_STATE_OF_HEADER':
      return state.map(header =>
        updatePropertyOnHeaderState(
          header,
          'isActive',
          action.data,
          action.index
        )
      );
    case 'RequestHeaders/REMOVE_HEADER_FROM_LIST':
      return state.filter(header => header.index !== action.index);
    case 'RequestHeaders/ADD_NEW_EMPTY_HEADER':
      return [...state, getEmptyHeader(state)];
    case 'RequestHeaders/SET_HEADERS_TO_DEFAULT_STATE':
      return defaultHeaderState;
    default:
      return state;
  }
};

export interface SetVariableValueText {
  type: 'RequestVariables/SET_HEADER_VALUE_TEXT';
  data: string;
  name: string;
}

export interface SetVariableStateToDefault {
  type: 'RequestVariables/SET_VARIABLES_TO_DEFAULT_STATE';
}

const updateVariableValue = (
  currentVariable: VariableState,
  updateName: string,
  value: string
) => {
  if (currentVariable.name !== updateName) {
    return currentVariable;
  }
  return { ...currentVariable, value };
};

export type VariableStateAction =
  | SetVariableStateToDefault
  | SetVariableValueText;

export const variableStateReducer = (
  state = defaultVariableState,
  action: VariableStateAction
): VariableState[] => {
  switch (action.type) {
    case 'RequestVariables/SET_HEADER_VALUE_TEXT':
      return state.map(variable =>
        updateVariableValue(variable, action.name, action.data)
      );
    case 'RequestVariables/SET_VARIABLES_TO_DEFAULT_STATE':
      return defaultVariableState;
    default:
      return state;
  }
};

export interface SetLoadingState {
  type: 'RequestLoadingState/SET_LOADING_STATE';
}

export interface SetRequestSuccess {
  type: 'RequestLoadingState/SET_REQUEST_SUCCESS';
  data: string;
}

export interface SetRequestErrored {
  type: 'RequestLoadingState/SET_REQUEST_ERRORED';
  data: {
    message: string;
    status: number;
  };
}

export type LoadingStateAction =
  | SetLoadingState
  | SetRequestSuccess
  | SetRequestErrored;

export const requestLoadingStateReducer = (
  state = defaultLoadingState,
  action: LoadingStateAction
) => {
  switch (action.type) {
    case 'RequestLoadingState/SET_LOADING_STATE':
      return {
        ...state,
        isLoading: true,
        error: undefined,
        data: undefined,
      };
    case 'RequestLoadingState/SET_REQUEST_SUCCESS':
      return {
        ...state,
        isLoading: false,
        data: action.data,
        success: true,
        error: undefined,
      };
    case 'RequestLoadingState/SET_REQUEST_ERRORED':
      return {
        ...state,
        data: undefined,
        isLoading: false,
        error: action.data,
        success: false,
      };
    default:
      return state;
  }
};
