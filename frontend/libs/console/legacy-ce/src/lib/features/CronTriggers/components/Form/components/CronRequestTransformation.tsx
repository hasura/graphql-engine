import React, { useEffect, useReducer } from 'react';
import ConfigureTransformation from '../../../../../components/Common/ConfigureTransformation/ConfigureTransformation';
import {
  getCronTriggerRequestTransformDefaultState,
  requestTransformReducer,
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
  setRequestTransformState,
  setRequestUrl,
  setRequestUrlError,
  setRequestUrlPreview,
  setRequestUrlTransform,
  setSessionVars,
} from '../../../../../components/Common/ConfigureTransformation/requestTransformState';
import {
  QueryParams,
  RequestTransform,
  RequestTransformContentType,
  RequestTransformMethod,
} from '../../../../../metadata/types';
import {
  KeyValuePair,
  RequestTransformStateBody,
} from '../../../../../components/Common/ConfigureTransformation/stateDefaults';
import {
  getRequestTransformObject,
  getTransformState,
  getValidateTransformOptions,
  parseValidateApiData,
} from '../../../../../components/Common/ConfigureTransformation/utils';
import { useDispatch } from 'react-redux';
import Endpoints from '../../../../../Endpoints';
import requestAction from '../../../../../utils/requestAction';
import { useDebouncedEffect } from '../../../../../hooks/useDebounceEffect';

interface CronRequestTransformationProps {
  initialValue?: RequestTransform;
  webhookUrl: string;
  payload: string;
  onChange: (data: RequestTransform) => void;
}

/**
 *
 * Using the legacy request transformation component to be consistent with other part of the console.
 * Replace with the new component once the it is ready.
 */

export const CronRequestTransformation = (
  props: CronRequestTransformationProps
) => {
  const { webhookUrl, payload, onChange, initialValue } = props;
  const webhook = {
    value: webhookUrl,
    type: 'url',
  };

  const [transformState, transformDispatch] = useReducer(
    requestTransformReducer,
    getCronTriggerRequestTransformDefaultState()
  );

  const resetSampleInput = () => {
    const value = {
      comment: 'test',
      id: '06af0430-e4d8-4335-8659-c27225e8edfd',
      name: 'test',
      payload: JSON.parse(payload || '{}'),
      scheduled_time: '2022-12-13T10:32:00Z',
    };
    transformDispatch(setRequestSampleInput(JSON.stringify(value, null, 2)));
  };

  useEffect(() => {
    resetSampleInput();
    if (initialValue) {
      transformDispatch(
        setRequestTransformState(
          getTransformState(initialValue, transformState.requestSampleInput)
        )
      );
    }
  }, [initialValue, transformDispatch]);

  const dispatch = useDispatch();

  const envVarsOnChange = (envVars: KeyValuePair[]) => {
    transformDispatch(setEnvVars(envVars));
  };

  const sessionVarsOnChange = (sessionVars: KeyValuePair[]) => {
    transformDispatch(setSessionVars(sessionVars));
  };

  const requestMethodOnChange = (requestMethod: RequestTransformMethod) => {
    transformDispatch(setRequestMethod(requestMethod));
  };

  const requestUrlOnChange = (requestUrl: string) => {
    transformDispatch(setRequestUrl(requestUrl));
  };

  const requestUrlErrorOnChange = (requestUrlError: string) => {
    transformDispatch(setRequestUrlError(requestUrlError));
  };

  const requestUrlPreviewOnChange = (requestUrlPreview: string) => {
    transformDispatch(setRequestUrlPreview(requestUrlPreview));
  };

  const requestQueryParamsOnChange = (requestQueryParams: QueryParams) => {
    transformDispatch(setRequestQueryParams(requestQueryParams));
  };

  const requestAddHeadersOnChange = (requestAddHeaders: KeyValuePair[]) => {
    transformDispatch(setRequestAddHeaders(requestAddHeaders));
  };

  const requestBodyOnChange = (requestBody: RequestTransformStateBody) => {
    transformDispatch(setRequestBody(requestBody));
  };

  const requestBodyErrorOnChange = (requestBodyError: string) => {
    transformDispatch(setRequestBodyError(requestBodyError));
  };

  const requestSampleInputOnChange = (requestSampleInput: string) => {
    transformDispatch(setRequestSampleInput(requestSampleInput));
  };

  const requestTransformedBodyOnChange = (requestTransformedBody: string) => {
    transformDispatch(setRequestTransformedBody(requestTransformedBody));
  };

  const requestContentTypeOnChange = (
    requestContentType: RequestTransformContentType
  ) => {
    transformDispatch(setRequestContentType(requestContentType));
  };

  const requestUrlTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestUrlTransform(data));
  };

  const requestPayloadTransformOnChange = (data: boolean) => {
    transformDispatch(setRequestPayloadTransform(data));
  };

  useDebouncedEffect(
    () => {
      requestUrlErrorOnChange('');
      requestUrlPreviewOnChange('');
      const onResponse = (data: Record<string, any>) => {
        parseValidateApiData(
          data,
          requestUrlErrorOnChange,
          requestUrlPreviewOnChange
        );
      };
      const options = getValidateTransformOptions({
        version: transformState.version,
        inputPayloadString: transformState.requestSampleInput,
        webhookUrl: webhook.value,
        envVarsFromContext: transformState.envVars,
        sessionVarsFromContext: transformState.sessionVars,
        requestUrl: transformState.requestUrl,
        queryParams: transformState.requestQueryParams,
        isEnvVar: webhook.type === 'env',
      });
      if (!webhook.value) {
        requestUrlErrorOnChange(
          'Please configure your webhook handler to generate request url transform'
        );
      } else if (
        transformState.requestSampleInput &&
        transformState.requestBody
      ) {
        (
          dispatch(
            requestAction(
              Endpoints.metadata,
              options,
              undefined,
              undefined,
              true,
              true
            )
          ) as unknown as Promise<Record<string, unknown>>
        ).then(onResponse, onResponse); // parseValidateApiData will parse both success and error
      }
    },

    500,
    [
      transformState.requestSampleInput,
      webhookUrl,
      transformState.requestUrl,
      transformState.requestQueryParams,
      transformState.envVars,
      transformState.sessionVars,
    ]
  );

  useDebouncedEffect(
    () => {
      requestBodyErrorOnChange('');
      requestTransformedBodyOnChange('');
      const onResponse = (data: Record<string, any>) => {
        parseValidateApiData(
          data,
          requestBodyErrorOnChange,
          undefined,
          requestTransformedBodyOnChange
        );
      };
      const options = getValidateTransformOptions({
        version: transformState.version,
        inputPayloadString: transformState.requestSampleInput,
        webhookUrl: webhook.value,
        envVarsFromContext: transformState.envVars,
        sessionVarsFromContext: transformState.sessionVars,
        transformerBody: transformState.requestBody,
        isEnvVar: webhook.type === 'env',
      });
      if (!webhook.value) {
        requestBodyErrorOnChange(
          'Please configure your webhook handler to generate request body transform'
        );
      } else if (
        transformState.requestSampleInput &&
        transformState.requestBody
      ) {
        (
          dispatch(
            requestAction(
              Endpoints.metadata,
              options,
              undefined,
              undefined,
              true,
              true
            )
          ) as unknown as Promise<Record<string, unknown>>
        ).then(onResponse, onResponse); // parseValidateApiData will parse both success and error
      }
    },
    500,
    [
      transformState.requestSampleInput,
      transformState.requestBody,
      webhookUrl,
      transformState.envVars,
      transformState.sessionVars,
    ]
  );

  useEffect(() => {
    const data = getRequestTransformObject(transformState);
    if (data) {
      onChange(data);
    }
  }, [transformState]);

  return (
    <ConfigureTransformation
      transformationType="event"
      requestTransfromState={transformState}
      resetSampleInput={resetSampleInput}
      envVarsOnChange={envVarsOnChange}
      sessionVarsOnChange={sessionVarsOnChange}
      requestMethodOnChange={requestMethodOnChange}
      requestUrlOnChange={requestUrlOnChange}
      requestQueryParamsOnChange={requestQueryParamsOnChange}
      requestAddHeadersOnChange={requestAddHeadersOnChange}
      requestBodyOnChange={requestBodyOnChange}
      requestSampleInputOnChange={requestSampleInputOnChange}
      requestContentTypeOnChange={requestContentTypeOnChange}
      requestUrlTransformOnChange={requestUrlTransformOnChange}
      requestPayloadTransformOnChange={requestPayloadTransformOnChange}
    />
  );
};
