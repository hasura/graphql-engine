import {
  Collapsible,
  CollapsibleHeader,
  IconTooltip,
  InputField,
  Text,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const RetryConfiguration = () => {
  return (
    <Collapsible
      data-testid="retry-configuration"
      triggerChildren={
        <CollapsibleHeader
          title="Retry Configuration"
          tooltip="Retry configuration if the call to the webhook fails"
        />
      }
    >
      <div className="space-y-2 relative max-w-(--breakpoint-lg)">
        <div className="grid grid-cols-12 gap-3 mb-2">
          <Flex align="center" className="col-span-6" gap="2">
            <Text as="p">Number of retries</Text>
            <IconTooltip message="Number of retries that Hasura makes to the webhook in case of failure" />
          </Flex>
          <div className="col-span-6">
            {/* TODO: This is a horizontal/inline input field, currently we do not have it in common so this component implements its own,
                 we should replace this in future with the common component */}
            <InputField
              data-testid="num_retries"
              aria-label="num_retries"
              name="num_retries"
              noErrorPlaceholder
              fieldProps={{
                type: 'number',
              }}
            />
          </div>
        </div>
        <div className="grid grid-cols-12 gap-3 mb-2">
          <Flex align="center" className="col-span-6" gap="2">
            <Text>Retry interval in seconds</Text>
            <IconTooltip message="Interval (in seconds) between each retry" />
          </Flex>
          <div className="col-span-6">
            <InputField
              data-testid="retry_interval_seconds"
              name="retry_interval_seconds"
              aria-label="retry_interval_seconds"
              noErrorPlaceholder
              fieldProps={{
                type: 'number',
              }}
            />
          </div>
        </div>
        <div className="grid grid-cols-12 gap-3">
          <Flex align="center" className="col-span-6" gap="2">
            <Text>Timeout in seconds</Text>
            <IconTooltip message="Request timeout for the webhook" />
          </Flex>
          <div className="col-span-6">
            <InputField
              data-testid="timeout_seconds"
              name="timeout_seconds"
              aria-label="timeout_seconds"
              noErrorPlaceholder
              fieldProps={{
                type: 'number',
              }}
            />
          </div>
        </div>
      </div>
    </Collapsible>
  );
};
