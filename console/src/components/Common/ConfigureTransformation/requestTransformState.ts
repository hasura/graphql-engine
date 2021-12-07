import {
  RequestTransformMethod,
  RequestTransformContentType,
} from '../../../metadata/types';
import {
  SET_REQUEST_METHOD,
  SET_REQUEST_URL,
  SET_REQUEST_URL_ERROR,
  SET_REQUEST_URL_PREVIEW,
  SET_REQUEST_QUERY_PARAMS,
  SET_REQUEST_ADD_HEADERS,
  SET_REQUEST_BODY,
  SET_REQUEST_BODY_ERROR,
  SET_REQUEST_SAMPLE_INPUT,
  SET_REQUEST_TRANSFORMED_BODY,
  SET_REQUEST_CONTENT_TYPE,
  SET_REQUEST_URL_TRANSFORM,
  SET_REQUEST_PAYLOAD_TRANSFORM,
  SET_REQUEST_TRANSFORM_STATE,
  SetRequestMethod,
  SetRequestUrl,
  SetRequestUrlError,
  SetRequestUrlPreview,
  SetRequestQueryParams,
  SetRequestAddHeaders,
  SetRequestBody,
  SetRequestBodyError,
  SetRequestSampleInput,
  SetRequestTransformedBody,
  SetRequestTransformState,
  SetRequestContentType,
  SetRequestUrlTransform,
  SetRequestPayloadTransform,
  RequestTransformEvents,
  defaultActionRequestSampleInput,
  defaultActionRequestBody,
  defaultActionRequestSamplePayload,
  defaultRequestContentType,
  RequestTransformState,
  KeyValuePair,
  defaultEventRequestBody,
  defaultEventRequestSampleInput,
} from './stateDefaults';

export const setRequestMethod = (
  requestMethod: RequestTransformMethod
): SetRequestMethod => ({
  type: SET_REQUEST_METHOD,
  requestMethod,
});

export const setRequestUrl = (requestUrl: string): SetRequestUrl => ({
  type: SET_REQUEST_URL,
  requestUrl,
});

export const setRequestUrlError = (
  requestUrlError: string
): SetRequestUrlError => ({
  type: SET_REQUEST_URL_ERROR,
  requestUrlError,
});

export const setRequestUrlPreview = (
  requestUrlPreview: string
): SetRequestUrlPreview => ({
  type: SET_REQUEST_URL_PREVIEW,
  requestUrlPreview,
});

export const setRequestQueryParams = (
  requestQueryParams: KeyValuePair[]
): SetRequestQueryParams => ({
  type: SET_REQUEST_QUERY_PARAMS,
  requestQueryParams,
});

export const setRequestAddHeaders = (
  requestAddHeaders: KeyValuePair[]
): SetRequestAddHeaders => ({
  type: SET_REQUEST_ADD_HEADERS,
  requestAddHeaders,
});

export const setRequestBody = (requestBody: string): SetRequestBody => ({
  type: SET_REQUEST_BODY,
  requestBody,
});

export const setRequestBodyError = (
  requestBodyError: string
): SetRequestBodyError => ({
  type: SET_REQUEST_BODY_ERROR,
  requestBodyError,
});

export const setRequestSampleInput = (
  requestSampleInput: string
): SetRequestSampleInput => ({
  type: SET_REQUEST_SAMPLE_INPUT,
  requestSampleInput,
});

export const setRequestTransformedBody = (
  requestTransformedBody: string
): SetRequestTransformedBody => ({
  type: SET_REQUEST_TRANSFORMED_BODY,
  requestTransformedBody,
});

export const setRequestContentType = (
  requestContentType: RequestTransformContentType
): SetRequestContentType => ({
  type: SET_REQUEST_CONTENT_TYPE,
  requestContentType,
});

export const setRequestUrlTransform = (
  isRequestUrlTransform: boolean
): SetRequestUrlTransform => ({
  type: SET_REQUEST_URL_TRANSFORM,
  isRequestUrlTransform,
});

export const setRequestPayloadTransform = (
  isRequestPayloadTransform: boolean
): SetRequestPayloadTransform => ({
  type: SET_REQUEST_PAYLOAD_TRANSFORM,
  isRequestPayloadTransform,
});

export const setRequestTransformState = (
  newState: RequestTransformState
): SetRequestTransformState => ({
  type: SET_REQUEST_TRANSFORM_STATE,
  newState,
});

export const requestTransformState: RequestTransformState = {
  requestMethod: null,
  requestUrl: '',
  requestUrlError: '',
  requestUrlPreview: '',
  requestQueryParams: [],
  requestAddHeaders: [],
  requestBody: '',
  requestBodyError: '',
  requestSampleInput: '',
  requestTransformedBody: '',
  requestContentType: defaultRequestContentType,
  isRequestUrlTransform: false,
  isRequestPayloadTransform: false,
  templatingEngine: 'Kriti',
};

export const getActionRequestTransformDefaultState = (): RequestTransformState => {
  return {
    ...requestTransformState,
    requestQueryParams: [{ name: '', value: '' }],
    requestAddHeaders: [{ name: '', value: '' }],
    requestBody: defaultActionRequestBody,
    requestSampleInput: defaultActionRequestSampleInput,
  };
};

export const getEventRequestTransformDefaultState = (): RequestTransformState => {
  return {
    ...requestTransformState,
    requestQueryParams: [{ name: '', value: '' }],
    requestAddHeaders: [{ name: '', value: '' }],
    requestBody: defaultEventRequestBody,
    requestSampleInput: defaultEventRequestSampleInput,
  };
};

export const requestTransformReducer = (
  state = requestTransformState,
  action: RequestTransformEvents
): RequestTransformState => {
  switch (action.type) {
    case SET_REQUEST_METHOD:
      return {
        ...state,
        requestMethod: action.requestMethod,
      };
    case SET_REQUEST_URL:
      return {
        ...state,
        requestUrl: action.requestUrl,
      };
    case SET_REQUEST_URL_ERROR:
      return {
        ...state,
        requestUrlError: action.requestUrlError,
      };
    case SET_REQUEST_URL_PREVIEW:
      return {
        ...state,
        requestUrlPreview: action.requestUrlPreview,
      };
    case SET_REQUEST_QUERY_PARAMS:
      return {
        ...state,
        requestQueryParams: action.requestQueryParams,
      };
    case SET_REQUEST_ADD_HEADERS:
      return {
        ...state,
        requestAddHeaders: action.requestAddHeaders,
      };
    case SET_REQUEST_BODY:
      return {
        ...state,
        requestBody: action.requestBody,
      };
    case SET_REQUEST_BODY_ERROR:
      return {
        ...state,
        requestBodyError: action.requestBodyError,
      };
    case SET_REQUEST_SAMPLE_INPUT:
      return {
        ...state,
        requestSampleInput: action.requestSampleInput,
      };
    case SET_REQUEST_TRANSFORMED_BODY:
      return {
        ...state,
        requestTransformedBody: action.requestTransformedBody,
      };
    case SET_REQUEST_CONTENT_TYPE:
      return {
        ...state,
        requestContentType: action.requestContentType,
        requestTransformedBody: defaultActionRequestSamplePayload(
          action.requestContentType
        ),
      };
    case SET_REQUEST_URL_TRANSFORM:
      return {
        ...state,
        isRequestUrlTransform: action.isRequestUrlTransform,
      };
    case SET_REQUEST_PAYLOAD_TRANSFORM:
      return {
        ...state,
        isRequestPayloadTransform: action.isRequestPayloadTransform,
      };
    case SET_REQUEST_TRANSFORM_STATE:
      return {
        ...action.newState,
      };
    default:
      return state;
  }
};
