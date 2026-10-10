import { InputField } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const RetryConfiguration = () => {
  return (
    <Flex direction="column" gap="4">
      <InputField
        name="num_retries"
        aria-label="num_retries"
        orientation="horizontal"
        label="Number of retries"
        tooltip="Number of retries that Hasura makes to the webhook in case of failure"
        fieldProps={{
          type: 'number',
        }}
      />
      <InputField
        name="retry_interval_seconds"
        aria-label="retry_interval_seconds"
        orientation="horizontal"
        label="Retry interval in seconds"
        tooltip="Interval (in seconds) between each retry"
        fieldProps={{
          type: 'number',
        }}
      />
      <InputField
        name="timeout_seconds"
        aria-label="timeout_seconds"
        orientation="horizontal"
        label="Timeout in seconds"
        tooltip="Request timeout for the webhook"
        fieldProps={{
          type: 'number',
        }}
      />
      <InputField
        name="tolerance_seconds"
        aria-label="tolerance_seconds"
        orientation="horizontal"
        label="Tolerance in seconds"
        tooltip="Number of seconds between scheduled time and actual delivery time that is acceptable"
        fieldProps={{
          type: 'number',
        }}
      />
    </Flex>
  );
};
