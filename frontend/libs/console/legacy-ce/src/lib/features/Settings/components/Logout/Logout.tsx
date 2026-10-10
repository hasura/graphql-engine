import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import ClearAdminSecret from './ClearAdminSecret';
import { Flex, Heading } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';

const Logout = () => {
  return (
    <Analytics name="Logout" {...REDACT_EVERYTHING}>
      <Flex className="p-4" direction="column" gap="4">
        <Heading size="4">Logout (clear admin-secret)</Heading>

        <Text className="w-8/12">
          The console caches the admin-secret (HASURA_GRAPHQL_ADMIN_SECRET) in
          the browser. You can clear this cache to force a prompt for the
          admin-secret when the console is accessed next using this browser.
        </Text>

        <div>
          <ClearAdminSecret />
        </div>
      </Flex>
    </Analytics>
  );
};

export default Logout;
