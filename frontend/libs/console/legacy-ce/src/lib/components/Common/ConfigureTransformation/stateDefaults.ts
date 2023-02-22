import { Action as ReduxAction } from 'redux';
import {
  getCronTriggerRequestSampleInput,
  getEventRequestSampleInput,
} from '../../Services/Events/EventTriggers/utils';
import { getActionRequestSampleInput } from '../../Services/Actions/Add/utils';
import {
  defaultActionDefSdl,
  defaultTypesDefSdl,
} from '../../Services/Actions/Common/stateDefaults';
import {
  RequestTransformMethod,
  RequestTransformContentType,
  RequestTransformTemplateEngine,
  RequestTransformBody,
  ResponseTransformBody,
  QueryParams,
} from '../../../metadata/types';
import { Nullable } from '../utils/tsUtils';

export const SET_ENV_VARS = 'RequestTransform/SET_ENV_VARS';
export const SET_SESSION_VARS = 'RequestTransform/SET_SESSION_VARS';
export const SET_REQUEST_METHOD = 'RequestTransform/SET_REQUEST_METHOD';
export const SET_REQUEST_URL = 'RequestTransform/SET_REQUEST_URL';
export const SET_REQUEST_URL_ERROR = 'RequestTransform/SET_REQUEST_URL_ERROR';
export const SET_REQUEST_URL_PREVIEW =
  'RequestTransform/SET_REQUEST_URL_PREVIEW';
export const SET_REQUEST_QUERY_PARAMS =
  'RequestTransform/SET_REQUEST_QUERY_PARAMS';
export const SET_REQUEST_ADD_HEADERS =
  'RequestTransform/SET_REQUEST_ADD_HEADERS';
export const SET_REQUEST_BODY = 'RequestTransform/SET_REQUEST_BODY';
export const SET_REQUEST_BODY_ERROR = 'RequestTransform/SET_REQUEST_BODY_ERROR';
export const SET_REQUEST_SAMPLE_INPUT =
  'RequestTransform/SET_REQUEST_SAMPLE_INPUT';
export const SET_REQUEST_TRANSFORMED_BODY =
  'RequestTransform/SET_REQUEST_TRANSFORMED_BODY';
export const SET_ENABLE_REQUEST_BODY =
  'RequestTransform/SET_ENABLE_REQUEST_BODY';
export const SET_REQUEST_CONTENT_TYPE =
  'RequestTransform/SET_REQUEST_CONTENT_TYPE';
export const SET_REQUEST_URL_TRANSFORM =
  'RequestTransform/SET_REQUEST_URL_TRANSFORM';
export const SET_REQUEST_PAYLOAD_TRANSFORM =
  'RequestTransform/SET_REQUEST_PAYLOAD_TRANSFORM';
export const SET_REQUEST_TRANSFORM_STATE =
  'RequestTransform/SET_REQUEST_TRANSFORM_STATE';
export const SET_RESPONSE_BODY = 'ResponseTransform/SET_RESPONSE_BODY';
export const SET_RESPONSE_PAYLOAD_TRANSFORM =
  'ResponseTransform/SET_RESPONSE_PAYLOAD_TRANSFORM';
export const SET_RESPONSE_TRANSFORM_STATE =
  'RequestTransform/SET_REQUEST_TRANSFORM_STATE';

export interface SetEnvVars extends ReduxAction {
  type: typeof SET_ENV_VARS;
  envVars: KeyValuePair[];
}

export interface SetSessionVars extends ReduxAction {
  type: typeof SET_SESSION_VARS;
  sessionVars: KeyValuePair[];
}

export interface SetRequestMethod extends ReduxAction {
  type: typeof SET_REQUEST_METHOD;
  requestMethod: RequestTransformMethod;
}

export interface SetRequestUrl extends ReduxAction {
  type: typeof SET_REQUEST_URL;
  requestUrl: string;
}

export interface SetRequestUrlError extends ReduxAction {
  type: typeof SET_REQUEST_URL_ERROR;
  requestUrlError: string;
}

export interface SetRequestUrlPreview extends ReduxAction {
  type: typeof SET_REQUEST_URL_PREVIEW;
  requestUrlPreview: string;
}

export interface SetRequestQueryParams extends ReduxAction {
  type: typeof SET_REQUEST_QUERY_PARAMS;
  requestQueryParams: QueryParams;
}

export interface SetRequestAddHeaders extends ReduxAction {
  type: typeof SET_REQUEST_ADD_HEADERS;
  requestAddHeaders: KeyValuePair[];
}

export interface SetRequestBody extends ReduxAction {
  type: typeof SET_REQUEST_BODY;
  requestBody: RequestTransformStateBody;
}

export interface SetResponseBody extends ReduxAction {
  type: typeof SET_RESPONSE_BODY;
  responseBody: ResponseTransformStateBody;
}

export interface SetRequestBodyError extends ReduxAction {
  type: typeof SET_REQUEST_BODY_ERROR;
  requestBodyError: string;
}

export interface SetRequestSampleInput extends ReduxAction {
  type: typeof SET_REQUEST_SAMPLE_INPUT;
  requestSampleInput: string;
}

export interface SetRequestTransformedBody extends ReduxAction {
  type: typeof SET_REQUEST_TRANSFORMED_BODY;
  requestTransformedBody: string;
}

export interface SetRequestContentType extends ReduxAction {
  type: typeof SET_REQUEST_CONTENT_TYPE;
  requestContentType: RequestTransformContentType;
}

export interface SetRequestUrlTransform extends ReduxAction {
  type: typeof SET_REQUEST_URL_TRANSFORM;
  isRequestUrlTransform: boolean;
}

export interface SetRequestPayloadTransform extends ReduxAction {
  type: typeof SET_REQUEST_PAYLOAD_TRANSFORM;
  isRequestPayloadTransform: boolean;
}

export interface SetResponsePayloadTransform extends ReduxAction {
  type: typeof SET_RESPONSE_PAYLOAD_TRANSFORM;
  isResponsePayloadTransform: boolean;
}

export interface SetRequestTransformState extends ReduxAction {
  type: typeof SET_REQUEST_TRANSFORM_STATE;
  newState: RequestTransformState;
}

export interface SetResponseTransformState extends ReduxAction {
  type: typeof SET_RESPONSE_TRANSFORM_STATE;
  newState: ResponseTransformState;
}

export type RequestTransformEvents =
  | SetEnvVars
  | SetSessionVars
  | SetRequestMethod
  | SetRequestUrl
  | SetRequestUrlError
  | SetRequestUrlPreview
  | SetRequestQueryParams
  | SetRequestAddHeaders
  | SetRequestBody
  | SetRequestBodyError
  | SetRequestSampleInput
  | SetRequestTransformedBody
  | SetRequestContentType
  | SetRequestUrlTransform
  | SetRequestPayloadTransform
  | SetRequestTransformState;

export type ResponseTransformEvents =
  | SetResponseBody
  | SetResponsePayloadTransform
  | SetResponseTransformState;

export type RequestTransformState = {
  version: 1 | 2;
  envVars: KeyValuePair[];
  sessionVars: KeyValuePair[];
  requestMethod: Nullable<RequestTransformMethod>;
  requestUrl: string;
  requestUrlError: string;
  requestUrlPreview: string;
  requestQueryParams: QueryParams;
  requestAddHeaders: KeyValuePair[];
  requestBody: RequestTransformStateBody;
  requestBodyError: string;
  requestSampleInput: string;
  requestTransformedBody: string;
  requestContentType: RequestTransformContentType;
  isRequestUrlTransform: boolean;
  isRequestPayloadTransform: boolean;
  templatingEngine: RequestTransformTemplateEngine;
};

export type ResponseTransformState = {
  version: 1 | 2;
  templatingEngine: RequestTransformTemplateEngine;
  isResponsePayloadTransform: boolean;
  responseBody: ResponseTransformStateBody;
};

export type RequestTransformStateBody = Omit<
  RequestTransformBody,
  'form_template'
> & { form_template?: KeyValuePair[] };

export type ResponseTransformStateBody = Omit<
  ResponseTransformBody,
  'form_template'
> & { form_template?: KeyValuePair[] };

export type KeyValuePair = {
  name: string;
  value: string;
};

export type GraphiQlHeader = {
  key: string;
  value: string;
  isActive: boolean;
  isNewHeader: boolean;
  isDisabled: boolean;
};

export type TransformationType = 'action' | 'event';

export const defaultActionRequestSampleInput = getActionRequestSampleInput(
  defaultActionDefSdl,
  defaultTypesDefSdl
);

export const defaultEventRequestSampleInput = getEventRequestSampleInput();

export const defaultCronTriggerSampleInput = getCronTriggerRequestSampleInput();

export const defaultActionRequestBody = `{
  "users": {
    "name": {{$body.input.arg1.username}},
    "password": {{$body.input.arg1.password}}
  }
}`;

export const defaultActionResponseBody = `{
  "response": {{$body}}
}`;

export const defaultEventRequestBody = `{
  "table": {
    "name": {{$body.table.name}},
    "schema": {{$body.table.schema}}
  }
}`;

export const defaultCronTriggerRequestBody = `{
  "payload": {{$body.payload}}
}`;

const defaultActionRequestJsonPayload = `{
  "users": {
    "name": "username",
    "password": "password",
  }
}`;

export const defaultActionRequestSamplePayload = (
  requestContentType: RequestTransformContentType
) => {
  if (requestContentType === 'application/json')
    return defaultActionRequestJsonPayload;

  if (requestContentType === 'application/x-www-form-urlencoded')
    return `retrieveSensitive=userId:1,username,password,role`;

  return '';
};

export const defaultRequestContentType: RequestTransformContentType =
  'application/json';
