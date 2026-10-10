import React from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';
import { Card, LearnMoreLink, Text } from '@hasura/shared/ui';
import { useEnvironmentState } from '../../hooks';
import { DbConnectConsoleType } from '../../types';
import { useAppContext } from '@hasura/shared/context';
import { Avatar, Code, Flex, Heading, Strong } from '@radix-ui/themes';

const DOC_LINK_PER_ENV: Record<
  'server' | 'cli',
  Record<DbConnectConsoleType, string | undefined>
> = {
  cli: {
    'pro-lite': undefined,
    oss: undefined,
    pro: undefined,
    cloud: undefined,
  },
  server: {
    'pro-lite':
      'https://hasura.io/docs/latest/deployment/deployment-guides/docker/#run-the-docker-container-with-an-admin-secret-env-var',
    oss: 'https://hasura.io/docs/latest/deployment/deployment-guides/docker/#run-the-docker-container-with-an-admin-secret-env-var',
    pro: 'https://hasura.io/docs/latest/deployment/deployment-guides/docker/#run-the-docker-container-with-an-admin-secret-env-var',
    cloud: 'https://hasura.io/docs/latest/hasura-cloud/projects/env-vars/',
  },
};

const WarningAvatar = () => (
  <Avatar
    variant="soft"
    radius="full"
    color="amber"
    size="4"
    fallback={<FaExclamationTriangle size="28" />}
  />
);

export const WarningCard: React.FC<unknown> = () => {
  const { consoleType } = useEnvironmentState();
  const { envVars } = useAppContext();
  const docLink = DOC_LINK_PER_ENV[envVars.consoleMode][consoleType];

  return (
    <Card mode="warning" className="mb-3">
      <Flex gap="4" align="center">
        <WarningAvatar />
        <div>
          <Heading size="3" color="amber">
            Warning
          </Heading>
          <Text as="div">
            This option <Strong>exposes sensitive information</Strong> such as
            password and hostname <Strong>in your metadata as plaintext</Strong>
            .
            <br />
            The <Strong>recommended way</Strong> of adding connections is using
            an <Strong>Environment variable</Strong>.
            {docLink ? <LearnMoreLink href={docLink} /> : null}
          </Text>
        </div>
      </Flex>
    </Card>
  );
};

export const WarningCardMetadataDBNotDynamic: React.FC<unknown> = () => {
  return (
    <Card mode="warning" className="mb-3">
      <Flex gap="4" align="center">
        <WarningAvatar />
        <div>
          <Heading size="3" color="amber">
            Warning
          </Heading>
          <Text as="div">
            This will have no effect on your metadata database URI, which may
            have been initialized from <Code>HASURA_GRAPHQL_DATABASE_URL</Code>.
            If you need a dynamic URL for metadata as well, your administrator
            will need to set{' '}
            <Code>
              HASURA_GRAPHQL_METADATA_DATABASE_URL=dynamic-from-file:///path/to/file
            </Code>
          </Text>
        </div>
      </Flex>
    </Card>
  );
};
