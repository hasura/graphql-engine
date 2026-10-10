import { Text } from '@hasura/shared/ui';
import { Code } from '@radix-ui/themes';

const AdminSecretDisabledMessage = () => (
  <Text as="p" align="center">
    Admin secret is disabled.
    <br />
    Remove the environment variable{' '}
    <Code>HASURA_GRAPHQL_DISABLE_ADMIN_SECRET</Code> to enable admin secret.
  </Text>
);

export default AdminSecretDisabledMessage;
