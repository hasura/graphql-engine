import {
  RequestHeadersSelector,
  Collapsible,
  FieldLabel,
  SwitchField,
  Separator,
} from '@hasura/shared/ui';
import { RetryConfiguration } from './RetryConfiguration';
import { Heading } from '@radix-ui/themes';

export const AdvancedSettings = () => {
  return (
    <div className="my-4">
      <Collapsible
        triggerChildren={<Heading size="4">Advanced Settings</Heading>}
      >
        <div className="mb-2 w-full">
          <FieldLabel
            label="Headers"
            tooltip="Configure headers for the request to the webhook"
          />
          <RequestHeadersSelector
            name="headers"
            addButtonText="Add request headers"
          />
        </div>
        <Separator size="4" className="my-4" />
        <div className="mb-4">
          <div className="mb-4">
            <Heading size="3">Retry Configuration</Heading>
          </div>
          <RetryConfiguration />
        </div>
        <div>
          <SwitchField
            name="include_in_metadata"
            label="Include in Metadata"
            tooltip="If enabled, this cron trigger will be included in the metadata of GraphqL Engine i.e. it will be a part of the metadata that is exported as migrations"
            noErrorPlaceholder
          />
        </div>
      </Collapsible>
    </div>
  );
};
