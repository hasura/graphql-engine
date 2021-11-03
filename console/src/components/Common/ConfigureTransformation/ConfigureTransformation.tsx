import React from 'react';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@/metadata/types';
import { KeyValuePair, RequestTransformState } from './stateDefaults';
import RequestOptionsTransforms from './RequestOptionsTransforms';
import PayloadOptionsTransforms from './PayloadOptionsTransforms';
import Button from '../Button';
import AddIcon from '../Icons/Add';

type ConfigureTransformationProps = {
  webhookUrl: string;
  state: RequestTransformState;
  resetSampleInput: () => void;
  requestMethodOnChange: (requestMethod: RequestTransformMethod) => void;
  requestUrlOnChange: (requestUrl: string) => void;
  requestUrlErrorOnChange: (requestUrlError: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: KeyValuePair[]) => void;
  requestAddHeadersOnChange: (requestAddHeaders: KeyValuePair[]) => void;
  requestBodyOnChange: (requestBody: string) => void;
  requestBodyErrorOnChange: (requestBodyError: string) => void;
  requestSampleInputOnChange: (requestSampleInput: string) => void;
  requestContentTypeOnChange: (
    requestContentType: RequestTransformContentType
  ) => void;
  requestUrlTransformOnChange: (data: boolean) => void;
  requestPayloadTransformOnChange: (data: boolean) => void;
};

const ConfigureTransformation: React.FC<ConfigureTransformationProps> = ({
  webhookUrl,
  state,
  resetSampleInput,
  requestMethodOnChange,
  requestUrlOnChange,
  requestUrlErrorOnChange,
  requestQueryParamsOnChange,
  requestAddHeadersOnChange,
  requestBodyOnChange,
  requestBodyErrorOnChange,
  requestSampleInputOnChange,
  requestContentTypeOnChange,
  requestUrlTransformOnChange,
  requestPayloadTransformOnChange,
}) => {
  const {
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
    requestContentType,
    isRequestUrlTransform,
    isRequestPayloadTransform,
  } = state;

  const requestUrlTransformText = isRequestUrlTransform
    ? `Remove Request Options Transformation`
    : `Add Request Options Transformation`;

  const requestPayloadTransformText = isRequestPayloadTransform
    ? `Remove Payload Transformation`
    : `Add Payload Transformation`;

  return (
    <>
      <h2 className="text-lg font-semibold mb-sm flex items-center">
        Configure Transformations
      </h2>
      <div className="mb-lg">
        <label className="block text-gray-600 font-medium mb-xs">
          Request Options Transformation
        </label>
        <p className="text-sm text-gray-600 mb-sm">
          Transform your method and URL to adapt to your webhook&apos;s expected
          format.
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
            requestUrlErrorOnChange={requestUrlErrorOnChange}
            requestQueryParamsOnChange={requestQueryParamsOnChange}
            requestAddHeadersOnChange={requestAddHeadersOnChange}
          />
        ) : null}
      </div>

      <div className="mb-lg">
        <label className="block text-gray-600 font-medium mb-xs">
          Payload Transformation
        </label>
        <p className="text-sm text-gray-600 mb-sm">
          Transform your payload to adapt to your webhook&apos;s expected
          format.
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
            requestBody={requestBody}
            requestBodyError={requestBodyError}
            requestSampleInput={requestSampleInput}
            requestTransformedBody={requestTransformedBody}
            requestContentType={requestContentType}
            webhookUrl={webhookUrl}
            resetSampleInput={resetSampleInput}
            requestBodyOnChange={requestBodyOnChange}
            requestBodyErrorOnChange={requestBodyErrorOnChange}
            requestSampleInputOnChange={requestSampleInputOnChange}
            requestContentTypeOnChange={requestContentTypeOnChange}
          />
        ) : null}
      </div>
    </>
  );
};

export default ConfigureTransformation;
