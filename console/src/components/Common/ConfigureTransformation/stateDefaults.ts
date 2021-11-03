import { Action as ReduxAction } from 'redux';
import { getActionRequestSampleInput } from '../../Services/Actions/Add/utils';
import {
  defaultActionDefSdl,
  defaultTypesDefSdl,
} from '../../Services/Actions/Common/stateDefaults';
import {
  RequestTransformMethod,
  RequestTransformContentType,
  RequestTransformTemplateEngine,
} from '../../../metadata/types';
import { Nullable } from '../utils/tsUtils';

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
export const SET_REQUEST_CONTENT_TYPE =
  'RequestTransform/SET_REQUEST_CONTENT_TYPE';
export const SET_REQUEST_URL_TRANSFORM =
  'RequestTransform/SET_REQUEST_URL_TRANSFORM';
export const SET_REQUEST_PAYLOAD_TRANSFORM =
  'RequestTransform/SET_REQUEST_PAYLOAD_TRANSFORM';
export const SET_REQUEST_TRANSFORM_STATE =
  'RequestTransform/SET_REQUEST_TRANSFORM_STATE';

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
  requestQueryParams: KeyValuePair[];
}

export interface SetRequestAddHeaders extends ReduxAction {
  type: typeof SET_REQUEST_ADD_HEADERS;
  requestAddHeaders: KeyValuePair[];
}

export interface SetRequestBody extends ReduxAction {
  type: typeof SET_REQUEST_BODY;
  requestBody: string;
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

export interface SetRequestTransformState extends ReduxAction {
  type: typeof SET_REQUEST_TRANSFORM_STATE;
  newState: RequestTransformState;
}

export type RequestTransformEvents =
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

export type RequestTransformState = {
  requestMethod: Nullable<RequestTransformMethod>;
  requestUrl: string;
  requestUrlError: string;
  requestUrlPreview: string;
  requestQueryParams: KeyValuePair[];
  requestAddHeaders: KeyValuePair[];
  requestBody: string;
  requestBodyError: string;
  requestSampleInput: string;
  requestTransformedBody: string;
  requestContentType: RequestTransformContentType;
  isRequestUrlTransform: boolean;
  isRequestPayloadTransform: boolean;
  templatingEngine: RequestTransformTemplateEngine;
};

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

export const defaultActionRequestSampleInput = getActionRequestSampleInput(
  defaultActionDefSdl,
  defaultTypesDefSdl
);

export const defaultActionRequestBody = `{
  "users": {
    "name": {{$body.input.arg1.username}},
    "password": {{$body.input.arg1.password}}
  }
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
