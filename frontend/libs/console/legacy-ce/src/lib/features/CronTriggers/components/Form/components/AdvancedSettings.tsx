import React from 'react';
import { RequestHeadersSelector } from '../../../../../new-components/RequestHeadersSelector';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { Collapsible } from '../../../../../new-components/Collapsible';
import { IncludeInMetadataSwitch } from './IncludeInMetadataSwitch';
import { RetryConfiguration } from './RetryConfiguration';

export const AdvancedSettings = () => {
  return (
    <div className="my-md">
      <Collapsible
        triggerChildren={
          <h2 className="text-lg font-semibold mb-xs flex items-center mb-0">
            Advanced Settings
          </h2>
        }
      >
        <div className="mb-xs">
          <label className="block flex items-center text-gray-600 font-semibold mb-xs">
            Headers
            <IconTooltip message="Configure headers for the request to the webhook" />
          </label>
          <RequestHeadersSelector
            name="headers"
            addButtonText="Add request headers"
          />
        </div>
        <hr className="my-md" />
        <div className="mb-xs">
          <h4
            className="text-lg font-bold pb-md mt-0 mb-0
"
          >
            Retry Configuration
          </h4>
          <RetryConfiguration />
        </div>
        <hr className="my-md" />
        <div className="mb-xs">
          <IncludeInMetadataSwitch />
        </div>
      </Collapsible>
    </div>
  );
};
