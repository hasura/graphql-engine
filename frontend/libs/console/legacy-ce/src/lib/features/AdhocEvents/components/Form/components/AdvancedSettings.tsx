import React from 'react';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { Collapse } from '../../../../../new-components/deprecated';
import { RequestHeadersSelector } from '../../../../../new-components/RequestHeadersSelector';

export const AdvancedSettings = () => {
  return (
    <Collapse title="Advance Settings">
      <Collapse.Content>
        <div className="relative max-w-xl">
          <div className="mb-md">
            <label className="block flex items-center text-gray-600 font-semibold mb-xs">
              Headers
              <IconTooltip message="Configure headers for the request to the webhook" />
            </label>
            <RequestHeadersSelector
              name="headers"
              addButtonText="Add request headers"
            />
          </div>
        </div>
      </Collapse.Content>
    </Collapse>
  );
};
