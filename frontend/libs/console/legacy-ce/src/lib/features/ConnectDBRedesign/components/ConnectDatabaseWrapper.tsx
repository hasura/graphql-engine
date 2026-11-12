import { useMetadata } from '@hasura/metadata/api';
import React from 'react';
import { Flex, Heading } from '@radix-ui/themes';
import { DatabaseConnectIllustration } from './DatabaseConnectIllustration';
import { Separator, Text } from '@hasura/shared/ui';

export const ConnectDatabaseWrapper: React.FC<{
  children?: React.ReactNode;
}> = ({ children }) => {
  const { data: metadataSources } = useMetadata((m) => m.metadata.sources);

  return (
    <Flex direction="column" align="center">
      <Flex className="max-w-3xl w-full py-6">
        <div>
          <Heading size="6">
            {metadataSources?.length
              ? 'Connect Database'
              : 'Connect Your First Database'}
          </Heading>
          {metadataSources?.length ? (
            <Text>
              Connect a database to access your database objects in your GraphQL
              API.
            </Text>
          ) : (
            <Text>
              Connect your first database to access your database objects in
              your GraphQL API.
            </Text>
          )}
        </div>
      </Flex>
      <Separator size="4" className="mb-6" />
      <div className="max-w-3xl py-4 w-full">
        <Flex direction="column">
          <DatabaseConnectIllustration className="mb-4 w-full" />
          {children}
        </Flex>
      </div>
    </Flex>
  );
};
