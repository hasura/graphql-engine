import { Flex, Strong } from '@radix-ui/themes';
import { HasuraOnboarding } from './HasuraOnboardingSVG';
import { Card, Text } from '@hasura/shared/ui';

export function OnboardingAnimation() {
  return (
    <Card>
      <Flex
        direction="column"
        justify="center"
        align="center"
        className="overflow-auto mb-4"
        gap="4"
      >
        <div>
          <HasuraOnboarding />
        </div>
        <Text as="div">
          We recommend connecting a database to instantly explore the
          auto-generated <Strong>Hasura GraphQL API</Strong>
        </Text>
      </Flex>
    </Card>
  );
}
