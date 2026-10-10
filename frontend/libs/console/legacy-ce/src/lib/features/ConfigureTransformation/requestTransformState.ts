import { NameValue } from '@hasura/shared/types';
import {
  SET_ENV_VARS,
  SET_SESSION_VARS,
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
  SetSessionVars,
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
  defaultActionRequestSamplePayload,
  defaultRequestContentType,
  RequestTransformState,
  RequestTransformStateBody,
  SET_RESPONSE_PAYLOAD_TRANSFORM,
  SetResponsePayloadTransform,
  ResponseTransformStateBody,
  SET_RESPONSE_BODY,
  SetResponseBody,
  ResponseTransformState,
  ResponseTransformEvents,
  SET_RESPONSE_TRANSFORM_STATE,
  SetResponseTransformState,
  SetEnvVar,
  QueryParams,
} from './stateDefaults';
import {
  RequestTransformBodyActions,
  RequestTransformContentType,
  RequestTransformMethod,
} from '@hasura/shared/types';

export const setEnvVars = (envVars: NameValue[]): SetEnvVar => ({
  type: SET_ENV_VARS,
  envVars,
});

export const setSessionVars = (sessionVars: NameValue[]): SetSessionVars => ({
  type: SET_SESSION_VARS,
  sessionVars,
});

export const setRequestMethod = (
  requestMethod: RequestTransformMethod,
): SetRequestMethod => ({
  type: SET_REQUEST_METHOD,
  requestMethod,
});

export const setRequestUrl = (requestUrl: string): SetRequestUrl => ({
  type: SET_REQUEST_URL,
  requestUrl,
});

export const setRequestUrlError = (
  requestUrlError: string,
): SetRequestUrlError => ({
  type: SET_REQUEST_URL_ERROR,
  requestUrlError,
});

export const setRequestUrlPreview = (
  requestUrlPreview: string,
): SetRequestUrlPreview => ({
  type: SET_REQUEST_URL_PREVIEW,
  requestUrlPreview,
});

export const setRequestQueryParams = (
  requestQueryParams: QueryParams,
): SetRequestQueryParams => ({
  type: SET_REQUEST_QUERY_PARAMS,
  requestQueryParams,
});

export const setRequestAddHeaders = (
  requestAddHeaders: NameValue[],
): SetRequestAddHeaders => ({
  type: SET_REQUEST_ADD_HEADERS,
  requestAddHeaders,
});

export const setRequestBody = (
  requestBody: RequestTransformStateBody,
): SetRequestBody => ({
  type: SET_REQUEST_BODY,
  requestBody,
});

export const setResponseBody = (
  responseBody: ResponseTransformStateBody,
): SetResponseBody => ({
  type: SET_RESPONSE_BODY,
  responseBody,
});

export const setRequestBodyError = (
  requestBodyError: string,
): SetRequestBodyError => ({
  type: SET_REQUEST_BODY_ERROR,
  requestBodyError,
});

export const setRequestSampleInput = (
  requestSampleInput: string,
): SetRequestSampleInput => ({
  type: SET_REQUEST_SAMPLE_INPUT,
  requestSampleInput,
});

export const setRequestTransformedBody = (
  requestTransformedBody: string,
): SetRequestTransformedBody => ({
  type: SET_REQUEST_TRANSFORMED_BODY,
  requestTransformedBody,
});

export const setRequestContentType = (
  requestContentType: RequestTransformContentType,
): SetRequestContentType => ({
  type: SET_REQUEST_CONTENT_TYPE,
  requestContentType,
});

export const setRequestUrlTransform = (
  isRequestUrlTransform: boolean,
): SetRequestUrlTransform => ({
  type: SET_REQUEST_URL_TRANSFORM,
  isRequestUrlTransform,
});

export const setRequestPayloadTransform = (
  isRequestPayloadTransform: boolean,
): SetRequestPayloadTransform => ({
  type: SET_REQUEST_PAYLOAD_TRANSFORM,
  isRequestPayloadTransform,
});

export const setResponsePayloadTransform = (
  isResponsePayloadTransform: boolean,
): SetResponsePayloadTransform => ({
  type: SET_RESPONSE_PAYLOAD_TRANSFORM,
  isResponsePayloadTransform,
});

export const setRequestTransformState = (
  newState: RequestTransformState,
): SetRequestTransformState => ({
  type: SET_REQUEST_TRANSFORM_STATE,
  newState,
});

export const setResponseTransformState = (
  newState: ResponseTransformState,
): SetResponseTransformState => ({
  type: SET_RESPONSE_TRANSFORM_STATE,
  newState,
});

export const requestBodyActionState = {
  remove: 'remove' as const,
  transformApplicationJson: 'transform' as const,
  transformFormUrlEncoded: 'x_www_form_urlencoded' as const,
};

export const responseBodyActionState = {
  transformApplicationJson: 'transform' as RequestTransformBodyActions,
};

export const requestTransformState: RequestTransformState = {
  version: '2',
  envVars: [],
  sessionVars: [],
  requestMethod: null,
  requestUrl: '',
  requestUrlError: '',
  requestUrlPreview: '',
  requestQueryParams: [],
  requestAddHeaders: [],
  requestBody: { action: requestBodyActionState.transformApplicationJson },
  requestBodyError: '',
  requestSampleInput: '',
  requestTransformedBody: '',
  requestContentType: defaultRequestContentType,
  isRequestUrlTransform: false,
  isRequestPayloadTransform: false,
  templatingEngine: 'Kriti',
};

export const responseTransformState: ResponseTransformState = {
  version: 2,
  isResponsePayloadTransform: false,
  responseBody: { action: responseBodyActionState.transformApplicationJson },
  templatingEngine: 'Kriti',
};

export const requestTransformReducer = (
  state = requestTransformState,
  action: RequestTransformEvents,
): RequestTransformState => {
  switch (action.type) {
    case SET_ENV_VARS:
      return {
        ...state,
        envVars: action.envVars,
      };
    case SET_SESSION_VARS:
      return {
        ...state,
        sessionVars: action.sessionVars,
      };
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
          action.requestContentType,
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

export const responseTransformReducer = (
  state = responseTransformState,
  action: ResponseTransformEvents,
): ResponseTransformState => {
  switch (action.type) {
    case SET_RESPONSE_BODY:
      return {
        ...state,
        responseBody: action.responseBody,
      };
    case SET_RESPONSE_PAYLOAD_TRANSFORM:
      return {
        ...state,
        isResponsePayloadTransform: action.isResponsePayloadTransform,
      };
    case SET_RESPONSE_TRANSFORM_STATE:
      return {
        ...action.newState,
      };
    default:
      return state;
  }
};
