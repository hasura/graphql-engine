import React, { useState } from 'react';
import {
  RequestTransformMethod,
  RequestTransformContentType,
} from '@/metadata/types';
import {
  KeyValuePair,
  RequestTransformState,
  RequestTransformStateBody,
  TransformationType,
} from './stateDefaults';
import RequestOptionsTransforms from './RequestOptionsTransforms';
import PayloadOptionsTransforms from './PayloadOptionsTransforms';
import SampleContextTransforms from './SampleContextTransforms';
import Button from '../Button';
import AddIcon from '../Icons/Add';

type ConfigureTransformationProps = {
  transformationType: TransformationType;
  state: RequestTransformState;
  resetSampleInput: () => void;
  envVarsOnChange: (envVars: KeyValuePair[]) => void;
  sessionVarsOnChange: (sessionVars: KeyValuePair[]) => void;
  requestMethodOnChange: (requestMethod: RequestTransformMethod) => void;
  requestUrlOnChange: (requestUrl: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: KeyValuePair[]) => void;
  requestAddHeadersOnChange: (requestAddHeaders: KeyValuePair[]) => void;
  requestBodyOnChange: (requestBody: RequestTransformStateBody) => void;
  requestSampleInputOnChange: (requestSampleInput: string) => void;
  requestContentTypeOnChange?: (
    requestContentType: RequestTransformContentType
  ) => void;
  requestUrlTransformOnChange: (data: boolean) => void;
  requestPayloadTransformOnChange: (data: boolean) => void;
};

const ConfigureTransformation: React.FC<ConfigureTransformationProps> = ({
  transformationType,
  state,
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
}) => {
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
  } = state;

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
        <Button
          color="white"
          size="sm"
          data-test="toggle-context-area"
          onClick={() => {
            toggleContextArea(!isContextAreaActive);
          }}
        >
          {!isContextAreaActive ? <AddIcon /> : null}
          {contextAreaText}
        </Button>

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
        <Button
          color="white"
          size="sm"
          data-test="toggle-request-transform"
          onClick={() => {
            requestUrlTransformOnChange(!isRequestUrlTransform);
            resetSampleInput();
          }}
        >
          {!isRequestUrlTransform ? <AddIcon /> : null}
          {requestUrlTransformText}
        </Button>

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
        <Button
          color="white"
          size="sm"
          data-test="toggle-payload-transform"
          onClick={() => {
            requestPayloadTransformOnChange(!isRequestPayloadTransform);
            resetSampleInput();
          }}
        >
          {!isRequestPayloadTransform ? <AddIcon /> : null}
          {requestPayloadTransformText}
        </Button>
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
    </>
  );
};

export default ConfigureTransformation;
