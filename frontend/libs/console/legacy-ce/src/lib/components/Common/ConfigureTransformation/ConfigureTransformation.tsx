import React, { useState } from 'react';
import {
  RequestTransformMethod,
  RequestTransformContentType,
  QueryParams,
} from '../../../metadata/types';
import { Button } from '../../../new-components/Button';
import { Analytics } from '../../../features/Analytics';
import {
  KeyValuePair,
  RequestTransformState,
  RequestTransformStateBody,
  ResponseTransformState,
  ResponseTransformStateBody,
  TransformationType,
} from './stateDefaults';
import RequestOptionsTransforms from './RequestOptionsTransforms';
import PayloadOptionsTransforms from './PayloadOptionsTransforms';
import SampleContextTransforms from './SampleContextTransforms';
import AddIcon from '../Icons/Add';
import ResponseTransforms from './ResponseTransform';

type ConfigureTransformationProps = {
  transformationType: TransformationType;
  requestTransfromState: RequestTransformState;
  responseTransformState?: ResponseTransformState;
  resetSampleInput: () => void;
  envVarsOnChange: (envVars: KeyValuePair[]) => void;
  sessionVarsOnChange: (sessionVars: KeyValuePair[]) => void;
  requestMethodOnChange: (requestMethod: RequestTransformMethod) => void;
  requestUrlOnChange: (requestUrl: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: QueryParams) => void;
  requestAddHeadersOnChange: (requestAddHeaders: KeyValuePair[]) => void;
  requestBodyOnChange: (requestBody: RequestTransformStateBody) => void;
  requestSampleInputOnChange: (requestSampleInput: string) => void;
  requestContentTypeOnChange?: (
    requestContentType: RequestTransformContentType
  ) => void;
  requestUrlTransformOnChange: (data: boolean) => void;
  requestPayloadTransformOnChange: (data: boolean) => void;
  responsePayloadTransformOnChange?: (data: boolean) => void;
  responseBodyOnChange?: (responseBody: ResponseTransformStateBody) => void;
};

const ConfigureTransformation: React.FC<
  ConfigureTransformationProps
> = props => {
  const {
    transformationType,
    requestTransfromState,
    responseTransformState,
    resetSampleInput,
    envVarsOnChange,
    sessionVarsOnChange,
    requestMethodOnChange,
    requestUrlOnChange,
    requestQueryParamsOnChange,
    requestAddHeadersOnChange,
    requestBodyOnChange,
    requestSampleInputOnChange,
    requestUrlTransformOnChange,
    requestPayloadTransformOnChange,
    responsePayloadTransformOnChange,
    responseBodyOnChange,
  } = props;
  const {
    envVars,
    sessionVars,
    requestMethod,
    requestUrl,
    requestUrlError,
    requestUrlPreview,
    requestQueryParams,
    requestAddHeaders,
    requestBody,
    requestBodyError,
    requestSampleInput,
    requestTransformedBody,
    isRequestUrlTransform,
    isRequestPayloadTransform,
  } = requestTransfromState;
  const [isContextAreaActive, toggleContextArea] = useState<boolean>(false);

  const contextAreaText = isContextAreaActive
    ? `Hide Sample Context`
    : `Show Sample Context`;

  const requestUrlTransformText = isRequestUrlTransform
    ? `Remove Request Options Transform`
    : `Add Request Options Transform`;
  const requestPayloadTransformText = isRequestPayloadTransform
    ? `Remove Payload Transform`
    : `Add Payload Transform`;

  const responsePayloadTransformText =
    responseTransformState?.isResponsePayloadTransform
      ? `Remove Response Transform`
      : `Add Response Transform`;
  return (
    <>
      <h2 className="text-lg font-semibold mb-sm flex items-center">
        Configure REST Connectors
      </h2>

      <div className="mb-lg">
        <label className="block text-gray-600 font-medium mb-xs">
          Sample Context
        </label>
        <p className="text-sm text-gray-600 mb-sm">
          Add sample env vars and session vars for testing the connector
        </p>
        <Analytics
          name={
            isContextAreaActive
              ? 'actions-tab-hide-sample-context-button'
              : 'actions-tab-show-sample-context-button'
          }
          passHtmlAttributesToChildren
        >
          <Button
            color="white"
            size="sm"
            onClick={() => {
              toggleContextArea(!isContextAreaActive);
            }}
          >
            {!isContextAreaActive ? <AddIcon /> : null}
            {contextAreaText}
          </Button>
        </Analytics>

        {isContextAreaActive ? (
          <SampleContextTransforms
            transformationType={transformationType}
            envVars={envVars}
            sessionVars={sessionVars}
            envVarsOnChange={envVarsOnChange}
            sessionVarsOnChange={sessionVarsOnChange}
          />
        ) : null}
      </div>

      <div className="mb-lg">
        <label className="block text-gray-600 font-medium mb-xs">
          Change Request Options
        </label>
        <p className="text-sm text-gray-600 mb-sm">
          Change the method and URL to adapt to your API&apos;s expected format.
        </p>
        <Analytics
          name={
            isRequestUrlTransform
              ? 'actions-tab-hide-request-transform-button'
              : 'actions-tab-show-request-transform-button'
          }
          passHtmlAttributesToChildren
        >
          <Button
            size="sm"
            icon={!isRequestUrlTransform ? <AddIcon /> : undefined}
            iconPosition="start"
            onClick={() => {
              requestUrlTransformOnChange(!isRequestUrlTransform);
              resetSampleInput();
            }}
          >
            {requestUrlTransformText}
          </Button>
        </Analytics>
        {isRequestUrlTransform ? (
          <RequestOptionsTransforms
            requestMethod={requestMethod}
            requestUrl={requestUrl}
            requestUrlError={requestUrlError}
            requestUrlPreview={requestUrlPreview}
            requestQueryParams={requestQueryParams}
            requestAddHeaders={requestAddHeaders}
            requestMethodOnChange={requestMethodOnChange}
            requestUrlOnChange={requestUrlOnChange}
            requestQueryParamsOnChange={requestQueryParamsOnChange}
            requestAddHeadersOnChange={requestAddHeadersOnChange}
          />
        ) : null}
      </div>

      <div className="mb-lg">
        <label className="block text-gray-600 font-medium mb-xs">
          Change Payload
        </label>
        <p className="text-sm text-gray-600 mb-sm">
          Change the payload to adapt to your API&apos;s expected format.
        </p>
        <Analytics
          name={
            isRequestPayloadTransform
              ? 'actions-tab-hide-payload-transform-button'
              : 'actions-tab-show-payload-transform-button'
          }
          passHtmlAttributesToChildren
        >
          <Button
            color="white"
            size="sm"
            onClick={() => {
              requestPayloadTransformOnChange(!isRequestPayloadTransform);
              resetSampleInput();
            }}
          >
            {!isRequestPayloadTransform ? <AddIcon /> : null}
            {requestPayloadTransformText}
          </Button>
        </Analytics>
        {isRequestPayloadTransform ? (
          <PayloadOptionsTransforms
            transformationType={transformationType}
            requestBody={requestBody}
            requestBodyError={requestBodyError}
            requestSampleInput={requestSampleInput}
            requestTransformedBody={requestTransformedBody}
            resetSampleInput={resetSampleInput}
            requestBodyOnChange={requestBodyOnChange}
            requestSampleInputOnChange={requestSampleInputOnChange}
          />
        ) : null}
      </div>
      {responseTransformState && responsePayloadTransformOnChange && (
        <div className="mb-lg">
          <label className="block text-gray-600 font-medium mb-xs">
            Change Response
          </label>
          <p className="text-sm text-gray-600 mb-sm">
            Change the incoming response to adapt to your declared types.
          </p>
          <Analytics
            name={
              isRequestPayloadTransform
                ? 'actions-tab-hide-response-transform-button'
                : 'actions-tab-show-response-transform-button'
            }
            passHtmlAttributesToChildren
          >
            <Button
              color="white"
              size="sm"
              onClick={() => {
                responsePayloadTransformOnChange(
                  !responseTransformState.isResponsePayloadTransform
                );
                resetSampleInput();
              }}
            >
              {!responseTransformState.isResponsePayloadTransform ? (
                <AddIcon />
              ) : null}
              {responsePayloadTransformText}
            </Button>
          </Analytics>
          {responseTransformState.isResponsePayloadTransform &&
          responseBodyOnChange ? (
            <ResponseTransforms
              responseBody={responseTransformState.responseBody}
              responseBodyOnChange={responseBodyOnChange}
            />
          ) : null}
        </div>
      )}
    </>
  );
};

export default ConfigureTransformation;
