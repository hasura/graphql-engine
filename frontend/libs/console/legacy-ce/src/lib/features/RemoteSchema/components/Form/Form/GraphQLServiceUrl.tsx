import {
  IconTooltip,
  InputField,
  LearnMoreLink,
  Text,
} from '@hasura/shared/ui';
import { FaShieldAlt } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

type Props = {
  disabled?: boolean;
  loading?: boolean;
};

export const GraphQLServiceUrl = ({ disabled, loading }: Props) => {
  return (
    <Flex direction="column" gap="2" className="mb-4 w-8/12">
      <Flex align="center" gap="2">
        <Text as="label" color="gray" weight="medium">
          GraphQL Service URL
        </Text>
        <IconTooltip
          message="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
          icon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
        />
        <LearnMoreLink href="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl" />
      </Flex>
      <Text as="p" size="1" className="text-sm text-gray-600 mb-2">
        Note: Provide an URL or use an env var to template the handler URL if
        you have different URLs for multiple environments.
      </Text>
      <InputField
        name="url.value"
        data-testid="url"
        loading={loading}
        fieldProps={{
          disabled,
          placeholder:
            'https://myservice.com/graphql or {{MY_WEBHOOK_URL}}/graphql',
        }}
      />
    </Flex>
  );
};
