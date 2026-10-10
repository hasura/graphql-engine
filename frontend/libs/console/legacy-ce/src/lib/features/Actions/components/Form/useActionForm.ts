import { useEffect, useReducer, useState } from 'react';
import {
  QueryParams,
  RequestTransformState,
  RequestTransformStateBody,
  ResponseTransformState,
  ResponseTransformStateBody,
} from '../../../ConfigureTransformation/stateDefaults';
import type { ActionExecution, ActionState } from '../../types';
import reducer, {
  setActionComment,
  setActionExecution,
  setActionHandler,
  setActionTimeout,
  setHeaders as dispatchNewHeaders,
  toggleForwardClientHeaders as toggleFCH,
  setActionDefinition,
  setTypeDefinition,
} from './reducer';
import {
  requestTransformReducer,
  responseTransformReducer,
  setEnvVars,
  setRequestAddHeaders,
  setRequestBody,
  setRequestBodyError,
  setRequestContentType,
  setRequestMethod,
  setRequestPayloadTransform,
  setRequestQueryParams,
  setRequestSampleInput,
  setRequestTransformedBody,
  setRequestUrl,
  setRequestUrlError,
  setRequestUrlPreview,
  setRequestUrlTransform,
  setResponseBody,
  setResponsePayloadTransform,
  setSessionVars,
} from '../../../ConfigureTransformation/requestTransformState';
import { GraphQLError } from 'graphql';
import { getActionRequestSampleInput } from './utils';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@hasura/shared/types';
import { getActionDefinitionFromSdl } from '../../../../shared/utils/sdlUtils';
import { useAppContext } from '@hasura/shared/context';
import { useTestWebhookTransform } from '../../../ConfigureTransformation';
import { parseValidateApiData } from '../../../ConfigureTransformation/utils';
import { NameValue, ClientHeader } from '@hasura/shared/types';

const useActionForm = (
  initialState: ActionState,
  initialRequestTransformState: RequestTransformState,
  initialResponseTransformState: ResponseTransformState,
) => {
  const testWebhookTransform = useTestWebhookTransform();
  const { readOnlyMode } = useAppContext();
  const [isFetching, setIsFetching] = useState(false);
  const [state, dispatch] = useReducer(reducer, initialState);
  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    initialRequestTransformState,
  );
  const [responseTransformState, responseTransformDispatch] = useReducer(
    responseTransformReducer,
    initialResponseTransformState,
  );

  // Declared before the effects below that consume them (they only dispatch to
  // the transform reducer, so hoisting is behaviour-preserving).
  const requestUrlErrorOnChange = (requestUrlError: string) => {
    transformDispatch(setRequestUrlError(requestUrlError));
  };

  const requestUrlPreviewOnChange = (requestUrlPreview: string) => {
    transformDispatch(setRequestUrlPreview(requestUrlPreview));
  };

  const requestBodyErrorOnChange = (requestBodyError: string) => {
    transformDispatch(setRequestBodyError(requestBodyError));
  };

  const requestTransformedBodyOnChange = (requestTransformedBody: string) => {
    transformDispatch(setRequestTransformedBody(requestTransformedBody));
  };

  // we send separate requests for the `url` preview and `body` preview, as in case of error,
  // we will not be able to resolve if the error is with url or body transform, with the current state of `test_webhook_transform` api
  useEffect(() => {
    requestUrlErrorOnChange('');
    requestUrlPreviewOnChange('');
    if (!state.handler) {
      requestUrlErrorOnChange(
        'Please configure your webhook handler to generate request url transform',
      );
    } else {
      const onResponse = (data: Record<string, any>) => {
        setIsFetching(false);
        parseValidateApiData(
          data,
          requestUrlErrorOnChange,
          requestUrlPreviewOnChange,
        );
      };

      setIsFetching(true);
      testWebhookTransform({
        version: transformState.version,
        inputPayloadString: transformState.requestSampleInput,
        webhookUrl: state.handler,
        envVarsFromContext: transformState.envVars,
        sessionVarsFromContext: transformState.sessionVars,
        requestUrl: transformState.requestUrl,
        queryParams: transformState.requestQueryParams,
      })
        .then(onResponse)
        .catch(onResponse);
    }
  }, [
    transformState.requestSampleInput,
    state.handler,
    transformState.requestUrl,
    transformState.requestQueryParams,
    transformState.envVars,
    transformState.sessionVars,
  ]);

  useEffect(() => {
    if (!state.handler) {
      requestBodyErrorOnChange(
        'Please configure your webhook handler to generate request body transform',
      );
    } else if (transformState.requestBody && state.handler) {
      const onResponse = (data: Record<string, any>) => {
        setIsFetching(false);
        parseValidateApiData(
          data,
          requestBodyErrorOnChange,
          undefined,
          requestTransformedBodyOnChange,
        );
      };

      setIsFetching(true);
      testWebhookTransform({
        version: transformState.version,
        inputPayloadString: transformState.requestSampleInput,
        webhookUrl: state.handler,
        envVarsFromContext: transformState.envVars,
        sessionVarsFromContext: transformState.sessionVars,
        requestUrl: transformState.requestUrl,
        queryParams: transformState.requestQueryParams,
        transformerBody: transformState.requestBody,
      })
        .then(onResponse)
        .catch(onResponse);
    }
  }, [
    transformState.requestSampleInput,
    transformState.requestBody,
    state.handler,
    transformState.envVars,
    transformState.sessionVars,
    transformState.requestTransformedBody,
  ]);

  const handlerOnChange = (val: string) => {
    dispatch(setActionHandler(val));
  };

  const executionOnChange = (k: ActionExecution) =>
    dispatch(setActionExecution(k));
  const timeoutOnChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    dispatch(setActionTimeout(e.target.value));
  const commentOnChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    dispatch(setActionComment(e.target.value));

  const setHeaders = (hs: ClientHeader[]) => {
    dispatch(dispatchNewHeaders(hs));
  };

  const toggleForwardClientHeaders = () => {
    dispatch(toggleFCH());
  };

  const actionDefinitionOnChange = (
    value: string | null,
    error: GraphQLError | null,
    timer: NodeJS.Timeout | null,
    ast: Record<string, any> | null,
  ) => {
    dispatch(setActionDefinition(value as string, error, timer, ast));
  };

  const typeDefinitionOnChange = (
    value: string | null,
    error: GraphQLError | null,
    timer: NodeJS.Timeout | null,
    ast: Record<string, any> | null,
  ) => {
    dispatch(setTypeDefinition(value as string, error, timer, ast));
  };

  // request transform methods
  const resetSampleInput = () => {
    if (!state.actionDefinition.error && !state.typeDefinition.error) {
      const value = getActionRequestSampleInput(
        state.actionDefinition.sdl,
        state.typeDefinition.sdl,
      );
      transformDispatch(setRequestSampleInput(JSON.stringify(value)));
    }
  };

  const envVarsOnChange = (envVars: NameValue[]) => {
    transformDispatch(setEnvVars(envVars));
  };

  const sessionVarsOnChange = (sessionVars: NameValue[]) => {
    transformDispatch(setSessionVars(sessionVars));
  };

  const requestMethodOnChange = (requestMethod: RequestTransformMethod) => {
    transformDispatch(setRequestMethod(requestMethod));
  };

  const requestUrlOnChange = (requestUrl: string) => {
    transformDispatch(setRequestUrl(requestUrl));
  };

  const requestQueryParamsOnChange = (requestQueryParams: QueryParams) => {
    transformDispatch(setRequestQueryParams(requestQueryParams));
  };

  const requestAddHeadersOnChange = (requestAddHeaders: NameValue[]) => {
    transformDispatch(setRequestAddHeaders(requestAddHeaders));
  };

  const requestBodyOnChange = (requestBody: RequestTransformStateBody) => {
    transformDispatch(setRequestBody(requestBody));
  };

  const requestSampleInputOnChange = (requestSampleInput: string) => {
    transformDispatch(setRequestSampleInput(requestSampleInput));
  };

  const requestContentTypeOnChange = (
    requestContentType: RequestTransformContentType,
  ) => {
    transformDispatch(setRequestContentType(requestContentType));
  };

  const requestUrlTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestUrlTransform(data));
  };

  const requestPayloadTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestPayloadTransform(data));
  };

  const responsePayloadTransformOnChange = (data: boolean) => {
    responseTransformDispatch(setResponsePayloadTransform(data));
  };

  const responseBodyOnChange = (responseBody: ResponseTransformStateBody) => {
    responseTransformDispatch(setResponseBody(responseBody));
  };

  const allowSave =
    !readOnlyMode &&
    !isFetching &&
    !state.typeDefinition.error &&
    !state.actionDefinition.error &&
    !state.actionDefinition.timer &&
    !state.typeDefinition.timer &&
    !readOnlyMode;

  let actionType = '';

  if (!state.actionDefinition.error && !state.actionDefinition.timer) {
    const { type, error } = getActionDefinitionFromSdl(
      state.actionDefinition.sdl,
    );
    if (!error) {
      actionType = type;
    }
  }

  return {
    state,
    isFetching,
    transformState,
    responseTransformState,
    handlerOnChange,
    executionOnChange,
    timeoutOnChange,
    commentOnChange,
    setHeaders,
    toggleForwardClientHeaders,
    actionDefinitionOnChange,
    typeDefinitionOnChange,
    resetSampleInput,
    envVarsOnChange,
    sessionVarsOnChange,
    requestMethodOnChange,
    requestUrlOnChange,
    requestQueryParamsOnChange,
    requestAddHeadersOnChange,
    requestBodyOnChange,
    requestSampleInputOnChange,
    requestContentTypeOnChange,
    requestUrlTransformOnChange,
    requestPayloadTransformOnChange,
    responsePayloadTransformOnChange,
    responseBodyOnChange,
    readOnlyMode,
    allowSave,
    actionType,
  };
};

export default useActionForm;
