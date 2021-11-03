import React from 'react';
import { RequestTransformMethod } from '../../../metadata/types';
import { KeyValuePair } from './stateDefaults';
import { focusYellowRing } from './utils';
import { Nullable } from '../utils/tsUtils';
import RequestUrlEditor from './CustomEditors/RequestUrlEditor';
import KeyValueInput from './CustomEditors/KeyValueInput';
import NumberedSidebar from './CustomEditors/NumberedSidebar';

type RequestOptionsTransformsProps = {
  requestMethod: Nullable<RequestTransformMethod>;
  requestUrl: string;
  requestUrlError: string;
  requestUrlPreview: string;
  requestQueryParams: KeyValuePair[];
  requestAddHeaders: KeyValuePair[];
  requestMethodOnChange: (requestMethod: RequestTransformMethod) => void;
  requestUrlOnChange: (requestUrl: string) => void;
  requestUrlErrorOnChange: (requestUrlError: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: KeyValuePair[]) => void;
  requestAddHeadersOnChange: (requestAddHeaders: KeyValuePair[]) => void;
};

const RequestOptionsTransforms: React.FC<RequestOptionsTransformsProps> = ({
  requestMethod,
  requestUrl,
  requestUrlError,
  requestUrlPreview,
  requestQueryParams,
  requestAddHeaders,
  requestMethodOnChange,
  requestUrlOnChange,
  requestUrlErrorOnChange,
  requestQueryParamsOnChange,
  requestAddHeadersOnChange,
}) => {
  const showRequestHeaders = false;
  const requestMethodOptions: RequestTransformMethod[] = [
    'GET',
    'POST',
    'PUT',
    'PATCH',
    'DELETE',
  ];

  return (
    <div className="m-md pl-lg pr-sm border-l border-l-gray-400">
      <div className="mb-md">
        <NumberedSidebar
          title="Request Method"
          url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#method"
          number="1"
        />
        {requestMethodOptions.map(method => (
          <div key={method} className="inline-flex items-center mr-md">
            <input
              id={method}
              name={method}
              type="radio"
              value={method}
              checked={requestMethod === method}
              onChange={() => requestMethodOnChange(method)}
              className={`mr-sm border-gray-400 ${focusYellowRing}`}
            />
            <label
              className="ml-sm"
              htmlFor={method}
              data-test={`transform-${method}`}
            >
              {method}
            </label>
          </div>
        ))}
      </div>

      <div className="mb-md">
        <NumberedSidebar
          title="Request URL Template"
          url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#url"
          number="2"
        />
        <RequestUrlEditor
          requestUrl={requestUrl}
          requestUrlError={requestUrlError}
          requestUrlPreview={requestUrlPreview}
          requestQueryParams={requestQueryParams}
          requestUrlOnChange={requestUrlOnChange}
          requestUrlErrorOnChange={requestUrlErrorOnChange}
          requestQueryParamsOnChange={requestQueryParamsOnChange}
        />
      </div>

      {showRequestHeaders ? (
        <div className="mb-md">
          <NumberedSidebar
            title="Configure Headers"
            url="https://hasura.io/docs/latest/graphql/core/actions/transforms.html#request-headers"
            number="3"
            description="Transform your request header into the required specification."
          />
          <div className="grid gap-3 grid-cols-3">
            <div>
              <label className="block text-gray-600 font-medium mb-xs">
                Add or Transform Header Key
              </label>
            </div>
            <div>
              <label className="block text-gray-600 font-medium mb-xs">
                Value
              </label>
            </div>
          </div>
          <div className="grid gap-3 grid-cols-3 mb-sm">
            <KeyValueInput
              pairs={requestAddHeaders}
              setPairs={requestAddHeadersOnChange}
            />
          </div>
        </div>
      ) : null}
    </div>
  );
};

export default RequestOptionsTransforms;
