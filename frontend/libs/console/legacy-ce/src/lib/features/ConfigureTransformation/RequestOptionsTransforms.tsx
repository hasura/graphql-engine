import React from 'react';
import RequestUrlEditor from './CustomEditors/RequestUrlEditor';
import KeyValueInput from './CustomEditors/KeyValueInput';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import { RadioGroup } from '@hasura/shared/ui';
import { NameValue } from '@hasura/shared/types';
import { QueryParams } from './stateDefaults';
import { RequestTransformMethod } from '@hasura/shared/types';

type RequestOptionsTransformsProps = {
  requestMethod: RequestTransformMethod | null;
  requestUrl: string;
  requestUrlError: string;
  requestUrlPreview: string;
  requestQueryParams: QueryParams;
  requestAddHeaders: NameValue[];
  requestMethodOnChange: (requestMethod: RequestTransformMethod) => void;
  requestUrlOnChange: (requestUrl: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: QueryParams) => void;
  requestAddHeadersOnChange: (requestAddHeaders: NameValue[]) => void;
};

const showRequestHeaders = false;
const requestMethodOptions = ['GET', 'POST', 'PUT', 'PATCH', 'DELETE'].map(
  (opt) => ({
    label: opt,
    value: opt,
  }),
);

const RequestOptionsTransforms: React.FC<RequestOptionsTransformsProps> = ({
  requestMethod,
  requestUrl,
  requestUrlError,
  requestUrlPreview,
  requestQueryParams,
  requestAddHeaders,
  requestMethodOnChange,
  requestUrlOnChange,
  requestQueryParamsOnChange,
  requestAddHeadersOnChange,
}) => {
  return (
    <div
      data-cy="Change Request Options"
      className="m-4 pl-10 border-l border-l-(--gray-a7)"
    >
      <div className="mb-4">
        <NumberedSidebar
          title="Request Method"
          number="1"
          url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#method"
        />
        <RadioGroup
          orientation="horizontal"
          options={requestMethodOptions}
          value={requestMethod}
          onChange={(value) =>
            requestMethodOnChange(value as RequestTransformMethod)
          }
        />
      </div>

      <div className="mb-4">
        <NumberedSidebar
          title="Request URL Template"
          number="2"
          url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#url"
        />
        <RequestUrlEditor
          requestUrl={requestUrl}
          requestUrlError={requestUrlError}
          requestUrlPreview={requestUrlPreview}
          requestQueryParams={requestQueryParams}
          requestUrlOnChange={requestUrlOnChange}
          requestQueryParamsOnChange={requestQueryParamsOnChange}
        />
      </div>

      {showRequestHeaders ? (
        <div className="mb-4">
          <NumberedSidebar
            title="Configure Headers"
            description="Transform your request header into the required specification."
            number="3"
            url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#request-headers"
          />
          <div className="grid gap-3 grid-cols-3">
            <div>
              <label className="block text-gray-600 font-medium mb-1">
                Add or Transform Header Key
              </label>
            </div>
            <div>
              <label className="block text-gray-600 font-medium mb-1">
                Value
              </label>
            </div>
          </div>
          <div className="grid gap-3 grid-cols-3 mb-2">
            <KeyValueInput
              pairs={requestAddHeaders}
              setPairs={requestAddHeadersOnChange}
              testId="add-headers"
            />
          </div>
        </div>
      ) : null}
    </div>
  );
};

export default RequestOptionsTransforms;
