import {
  Collapsible,
  CollapsibleHeader,
  RequestHeadersSelector,
  IconTooltip,
} from '@hasura/shared/ui';
import { Flex, Text } from '@radix-ui/themes';

export const AdvancedSettings = () => {
  return (
    <Collapsible
      triggerChildren={<CollapsibleHeader title="Advanced Settings" />}
    >
      <div className="relative max-w-8/12">
        <div className="mb-4">
          <Flex className="mb-2" align="center" gap="2">
            <Text as="label" color="gray" weight="bold">
              Headers
            </Text>
            <IconTooltip message="Configure headers for the request to the webhook" />
          </Flex>
          <RequestHeadersSelector
            name="headers"
            addButtonText="Add request headers"
          />
        </div>
      </div>
    </Collapsible>
  );
};
