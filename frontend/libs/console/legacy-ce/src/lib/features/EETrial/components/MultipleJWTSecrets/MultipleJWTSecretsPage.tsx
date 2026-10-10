import React from 'react';
import { MultipleJWTSecretsSvg } from './MultipleJWTSecretsSvg';
import { EETrialCard } from '../EETrialCard/EETrialCard';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import { Code, Flex, Heading } from '@radix-ui/themes';
import { LearnMoreLink, Text } from '@hasura/shared/ui';

export const MultipleJWTSecretsPage = () => {
  const { access } = useEELiteAccess();
  const isFeatureForbidden = access === 'forbidden';

  const isFeatureActive = access === 'active';

  if (isFeatureForbidden) return null;

  return (
    <Flex className="max-w-(--breakpoint-lg) p-4">
      <div className="max-w-3xl">
        <Heading size="5">Multiple JWT Secrets</Heading>
        <div className="mb-1">
          <Text>
            Enable access to your Hasura instance using multiple JSON web token
            secrets
          </Text>
          <LearnMoreLink
            href="https://hasura.io/docs/latest/security/multiple-jwt-secrets/"
            text="(Know More)"
          />
        </div>
        <MultipleJWTSecretsSvg />
        {isFeatureActive ? (
          <p className="mt-4">
            <Text weight="bold">Setup Multiple JWT Secrets</Text>
            <br />
            <LearnMoreLink
              text="Read more"
              href="https://hasura.io/docs/latest/security/multiple-jwt-secrets/"
              weight="bold"
            />{' '}
            on setting up multiple JWT secrets for your Hasura instance.
            <br />
            Multiple admin secrets may be enabled by setting the environment
            variable: <Code>HASURA_GRAPHQL_JWT_SECRETS</Code>
          </p>
        ) : (
          <EETrialCard
            id="multiple-jwt-secrets"
            className="mt-4"
            cardTitle="Want to enable multiple secrets for your instance?"
            cardText={
              <span>
                Get production-ready today with a 30-day free trial of Hasura
                EE, no credit card required.
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
