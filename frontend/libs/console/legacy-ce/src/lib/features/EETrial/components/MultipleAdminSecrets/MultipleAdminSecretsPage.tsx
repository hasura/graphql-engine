import { MultipleAdminSecretsSvg } from './MultipleAdminSecretsSvg';
import { Code, Flex, Heading } from '@radix-ui/themes';
import { EETrialCard } from '../EETrialCard/EETrialCard';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import { LearnMoreLink, Text } from '@hasura/shared/ui';

export const MultipleAdminSecretsPage = () => {
  const { access } = useEELiteAccess();
  const isFeatureForbidden = access === 'forbidden';

  const isFeatureActive = access === 'active';

  if (isFeatureForbidden) return null;

  return (
    <Flex className="max-w-(--breakpoint-lg) p-4">
      <div className="max-w-3xl">
        <Heading size="5">Multiple Admin Secrets</Heading>
        <div className="mb-1">
          <Text>
            Enable access to your Hasura instance using multiple
            x-hasura-admin-secrets.
          </Text>{' '}
          <LearnMoreLink
            href="https://hasura.io/docs/latest/security/multiple-admin-secrets/"
            text="(Know More)"
          />
        </div>
        <MultipleAdminSecretsSvg />
        {isFeatureActive ? (
          <div className="mt-4">
            <Text weight="bold">Setup Multiple Admin Secrets</Text>
            <Text as="p">
              <LearnMoreLink
                href="https://hasura.io/docs/latest/security/multiple-admin-secrets/"
                text="Read more"
                weight="bold"
              />{' '}
              on setting up multiple admin secrets for your Hasura instance.
              <br />
              Multiple admin secrets may be enabled by setting the environment
              variable: <Code>HASURA_GRAPHQL_ADMIN_SECRETS</Code>
            </Text>
          </div>
        ) : (
          <EETrialCard
            className="mt-4"
            id="multiple-admin-secrets"
            cardTitle="Want to enable multiple secrets for your instance?"
            cardText={
              <span>
                Implement security mechanisms like rotating secrets and have
                different lifecycles for individual admin secrets.
              </span>
            }
            buttonLabel="Enable Enterprise"
            eeAccess={access}
            horizontal
          />
        )}
      </div>
    </Flex>
  );
};
