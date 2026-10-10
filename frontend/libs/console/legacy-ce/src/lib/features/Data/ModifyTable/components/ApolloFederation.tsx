import { useUpdateApolloFederationConfig } from '../hooks/useUpdateApolloFederationConfig';
import {
  Card,
  LearnMoreLink,
  SkeletonList,
  Spinner,
  Switch,
  Text,
  hasuraToast,
} from '@hasura/shared/ui';
import {
  ServerConfig,
  useErrorNotification,
  useServerConfig,
} from '@hasura/metadata/api';
import { ModifyTableProps } from '../types';
import { Code, Flex, Heading } from '@radix-ui/themes';

const getIsApolloFlagEnabled = (data?: ServerConfig) =>
  data
    ? 'is_apollo_federation_enabled' in data &&
      data.is_apollo_federation_enabled
    : false;

export const ApolloFederation = ({ source, table }: ModifyTableProps) => {
  const showErrorNotification = useErrorNotification();

  /**
   * Check if the ENV variable is set on the server
   */
  const { data: isApolloFederationEnabled = false, isLoading } =
    useServerConfig(getIsApolloFlagEnabled);

  const { updateApolloConfig, isPending: updateInProgress } =
    useUpdateApolloFederationConfig({
      dataSourceName: source.name,
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Updated successfully!',
        });
      },
      onError: (err) => {
        showErrorNotification({
          title: 'Failed to update Apollo configuration',
          error: err,
        });
      },
    });

  if (isLoading) return <SkeletonList count={5} />;

  const apolloFederationConfig = table.apollo_federation_config;

  const handleToggle = () => {
    updateApolloConfig({
      table: table.table,
      isEnabled: !(apolloFederationConfig?.enable === 'v1'),
    });
  };

  return (
    <div>
      <Flex className="mb-2" gap="2" align="center">
        <Heading size="3">Enable Apollo Federation</Heading>
        <LearnMoreLink href="https://hasura.io/docs/latest/data-federation/apollo-federation/" />
      </Flex>
      <Card>
        <Flex align="center" justify="between">
          {!isApolloFederationEnabled ? (
            <Text>
              Apollo federation is not enabled. To enable apollo federation
              support, set the project env variable or start the Hasura server
              with environment variable{' '}
              <Code>
                HASURA_GRAPHQL_ENABLE_APOLLO_FEDERATION: &quot;true&quot;
              </Code>
            </Text>
          ) : (
            <>
              <Text>
                Enable Apollo Federation support to add Hasura as a subgraph in
                your Apollo federated gateway.
              </Text>
              <div>
                <div>
                  {updateInProgress ? (
                    <Spinner />
                  ) : (
                    <Switch
                      value={apolloFederationConfig?.enable === 'v1'}
                      onChange={() => handleToggle()}
                    />
                  )}
                </div>
              </div>
            </>
          )}
        </Flex>
      </Card>
    </div>
  );
};
