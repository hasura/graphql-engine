import React, { useState } from 'react';
import { Button, Text } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import {
  QueryParams,
  RequestTransformState,
  RequestTransformStateBody,
  ResponseTransformState,
  ResponseTransformStateBody,
  TransformationType,
} from './stateDefaults';
import RequestOptionsTransforms from './RequestOptionsTransforms';
import PayloadOptionsTransforms from './PayloadOptionsTransforms';
import SampleContextTransforms from './SampleContextTransforms';
import AddIcon from '../../components/Common/Icons/Add';
import ResponseTransforms from './ResponseTransform';
import { NameValue } from '@hasura/shared/types';
import {
  RequestTransformContentType,
  RequestTransformMethod,
} from '@hasura/shared/types';
import { Heading } from '@radix-ui/themes';

type ConfigureTransformationProps = {
  transformationType: TransformationType;
  requestTransformState: RequestTransformState;
  responseTransformState?: ResponseTransformState;
  resetSampleInput: () => void;
  envVarsOnChange: (envVars: NameValue[]) => void;
  sessionVarsOnChange: (sessionVars: NameValue[]) => void;
  requestMethodOnChange: (requestMethod: RequestTransformMethod) => void;
  requestUrlOnChange: (requestUrl: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: QueryParams) => void;
  requestAddHeadersOnChange: (requestAddHeaders: NameValue[]) => void;
  requestBodyOnChange: (requestBody: RequestTransformStateBody) => void;
  requestSampleInputOnChange: (requestSampleInput: string) => void;
  requestContentTypeOnChange?: (
    requestContentType: RequestTransformContentType,
  ) => void;
  requestUrlTransformOnChange: (data: boolean) => void;
  requestPayloadTransformOnChange: (data: boolean) => void;
  responsePayloadTransformOnChange?: (data: boolean) => void;
  responseBodyOnChange?: (responseBody: ResponseTransformStateBody) => void;
};

const ConfigureTransformation: React.FC<ConfigureTransformationProps> = ({
  transformationType,
  requestTransformState,
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
  } = requestTransformState;
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
      <Heading size="3">Configure REST Connectors</Heading>
      <div>
        <div className="my-4">
          <Text as="div" weight="medium">
            Sample Context
          </Text>
          <Text as="div" size="1">
            Add sample env vars and session vars for testing the connector
          </Text>
        </div>
        <Analytics
          name={
            isContextAreaActive
              ? 'actions-tab-hide-sample-context-button'
              : 'actions-tab-show-sample-context-button'
          }
          passHtmlAttributesToChildren
        >
          <Button
            mode="default"
            size="sm"
            onClick={() => {
              toggleContextArea(!isContextAreaActive);
            }}
            leftIcon={!isContextAreaActive ? AddIcon : undefined}
          >
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
      <div>
        <div className="my-4">
          <Text as="div" weight="medium">
            Change Request Options
          </Text>
          <Text as="div" size="1">
            Change the method and URL to adapt to your API&apos;s expected
            format.
          </Text>
        </div>
        <Analytics
          name={
            isRequestUrlTransform
              ? 'actions-tab-hide-request-transform-button'
              : 'actions-tab-show-request-transform-button'
          }
          passHtmlAttributesToChildren
        >
          <Button
            mode="default"
            size="1"
            leftIcon={!isRequestUrlTransform ? AddIcon : undefined}
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

      <div>
        <div className="my-4">
          <Text weight="medium" as="div">
            Change Payload
          </Text>
          <Text as="div" size="1">
            Change the payload to adapt to your API&apos;s expected format.
          </Text>
        </div>
        <Analytics
          name={
            isRequestPayloadTransform
              ? 'actions-tab-hide-payload-transform-button'
              : 'actions-tab-show-payload-transform-button'
          }
          passHtmlAttributesToChildren
        >
          <Button
            mode="default"
            size="sm"
            onClick={() => {
              requestPayloadTransformOnChange(!isRequestPayloadTransform);
              resetSampleInput();
            }}
            leftIcon={!isRequestPayloadTransform ? AddIcon : null}
          >
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
        <div className="my-4">
          <div className="mb-4">
            <Text weight="medium" as="div">
              Change Response
            </Text>
            <Text size="1" as="div">
              Change the incoming response to adapt to your declared types.
            </Text>
          </div>
          <Analytics
            name={
              isRequestPayloadTransform
                ? 'actions-tab-hide-response-transform-button'
                : 'actions-tab-show-response-transform-button'
            }
            passHtmlAttributesToChildren
          >
            <Button
              mode="default"
              size="sm"
              onClick={() => {
                responsePayloadTransformOnChange(
                  !responseTransformState.isResponsePayloadTransform,
                );
                resetSampleInput();
              }}
              leftIcon={
                !responseTransformState.isResponsePayloadTransform
                  ? AddIcon
                  : undefined
              }
            >
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
