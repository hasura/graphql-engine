import { AllowedRESTMethods } from '../../../../../metadata/types';
import { CreateEndpointState, defaultState } from './state';

export interface UpdateEndpointName {
  type: 'CreateEndpoint/UPDATE_ENDPOINT_NAME';
  data: string;
}

export interface UpdateEndpointComment {
  type: 'CreateEndpoint/UPDATE_ENDPOINT_COMMENT';
  data: string;
}

export interface UpdateEndpointURL {
  type: 'CreateEndpoint/UPDATE_ENDPOINT_URL';
  data: string;
}

export interface UpdateEndpointMethods {
  type: 'CreateEndpoint/UPDATE_ENDPOINT_METHODS';
  data: AllowedRESTMethods[];
}

export interface UpdateEndpointRequest {
  type: 'CreateEndpoint/UPDATE_ENDPOINT_REQUEST';
  data: string;
}

export interface ResetRESTInputState {
  type: 'CreateEndpoint/RESET_INPUT_STATE';
}

export type CreateEndpointAction =
  | UpdateEndpointName
  | UpdateEndpointComment
  | UpdateEndpointURL
  | UpdateEndpointMethods
  | UpdateEndpointRequest
  | ResetRESTInputState;

const createEndpointReducer = (
  state = defaultState,
  action: CreateEndpointAction
): CreateEndpointState => {
  switch (action.type) {
    case 'CreateEndpoint/UPDATE_ENDPOINT_NAME':
      return {
        ...state,
        name: action.data,
      };
    case 'CreateEndpoint/UPDATE_ENDPOINT_COMMENT':
      return {
        ...state,
        comment: action.data,
      };
    case 'CreateEndpoint/UPDATE_ENDPOINT_URL':
      return {
        ...state,
        url: action.data,
      };
    case 'CreateEndpoint/UPDATE_ENDPOINT_METHODS':
      return {
        ...state,
        methods: action.data,
      };
    case 'CreateEndpoint/UPDATE_ENDPOINT_REQUEST':
      return {
        ...state,
        request: action.data,
      };
    case 'CreateEndpoint/RESET_INPUT_STATE':
      return defaultState;
    default:
      return state;
  }
};

export default createEndpointReducer;
