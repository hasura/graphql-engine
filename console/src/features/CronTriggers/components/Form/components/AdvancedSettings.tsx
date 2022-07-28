import React from 'react';
import { ToolTip } from '@/new-components/Tooltip';
import { InputField, Radio } from '@/new-components/Form';
import { Collapse } from '@/new-components/Collapse';
import { RequestHeadersSelector } from '@/new-components/RequestHeadersSelector';

export const AdvancedSettings = () => {
  const requestMethodOptions = [
    { label: 'GET', value: 'GET' },
    { label: 'POST', value: 'POST' },
    { label: 'PUT', value: 'PUT' },
    { label: 'PATCH', value: 'PATCH' },
    { label: 'DELETE', value: 'DELETE' },
  ];

  return (
    <>
      <Collapse title="Header, URL, and Advanced Request Options">
        <Collapse.Content>
          <div className="relative max-w-xl">
            <div className="mb-md">
              <label className="block flex items-center text-gray-600 font-semibold mb-xs">
                Headers
                <ToolTip message="Configure headers for the request to the webhook" />
              </label>
              <RequestHeadersSelector
                name="headers"
                addButtonText="Add request headers"
              />
            </div>
            <div className="mb-md">
              <label className="block flex items-center text-gray-600 font-semibold">
                Method
                <ToolTip message="Configure method to transform your request to (optional). GET requests will be sent without a payload or content-type header" />
              </label>
              <Radio
                orientation="horizontal"
                name="request_method"
                options={requestMethodOptions}
              />
            </div>
            <div className="mb-md">
              <InputField
                name="url_template"
                prependLabel="{{$base_url}}"
                label="Request URL Template"
                placeholder="URL Template (Optional)..."
              />
            </div>
            <div className="mb-md">
              <label className="block flex items-center text-gray-600 font-semibold mb-xs">
                Query Params
                <ToolTip message="Configure headers for the request to the webhook" />
              </label>
              <RequestHeadersSelector
                name="query_params"
                typeSelect={false}
                addButtonText="Add query params"
              />
            </div>
          </div>
        </Collapse.Content>
      </Collapse>
    </>
  );
};
