import Skeleton from 'react-loading-skeleton';
import { ServerConfig, useServerConfig } from '../../../../hooks';
import { useUpdateApolloFederationConfig } from '../hooks/useUpdateApolloFederationConfig';
import { Table } from '../../../hasura-metadata-types';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { Switch } from '../../../../new-components/Switch';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import { CgSpinner } from 'react-icons/cg';
import { hasuraToast } from '../../../../new-components/Toasts';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';

const getIsApolloFlagEnabled = (data?: ServerConfig) =>
  data
    ? 'is_apollo_federation_enabled' in data &&
      data.is_apollo_federation_enabled
    : false;

export const ApolloFederation = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  /**
   * Check if the ENV variable is set on the server
   */
  const { data: isApolloFederationEnabled = false, isLoading } =
    useServerConfig(getIsApolloFlagEnabled);

  const { updateApolloConfig, isLoading: updateInProgress } =
    useUpdateApolloFederationConfig({
      dataSourceName,
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Updated successfully!',
        });
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: 'Failed to update Apollo configuration',
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });

  const { data: { apolloFederationConfig } = {} } = useMetadata(m => ({
    apolloFederationConfig: MetadataSelectors.findTable(
      dataSourceName,
      table
    )(m)?.apollo_federation_config,
  }));

  if (isLoading) return <Skeleton count={5} height={20} />;

  const handleToggle = () => {
    updateApolloConfig({
      table: table,
      isEnabled: !(apolloFederationConfig?.enable === 'v1'),
    });
  };

  return (
    <div>
      <div className="mb-sm">
        <span className="text-lg font-semibold">Enable Apollo Federation</span>
        <LearnMoreLink href="https://hasura.io/docs/latest/data-federation/apollo-federation/" />
      </div>
      <div className="bg-white border border-gray-300 flex items-center justify-between p-md rounded">
        {!isApolloFederationEnabled ? (
          <div>
            Apollo federation is not enabled. To enable apollo federation
            support, set the project env variable or start the Hasura server
            with environment variable
            <code className="bg-slate-100 rounded text-red-600">
              HASURA_GRAPHQL_ENABLE_APOLLO_FEDERATION: "true"
            </code>
          </div>
        ) : (
          <>
            <div>
              Enable Apollo Federation support to add Hasura as a subgraph in
              your Apollo federated gateway.
            </div>
            <div>
              <div>
                {updateInProgress ? (
                  <CgSpinner className="animate-spin" size={30} />
                ) : (
                  <Switch
                    checked={apolloFederationConfig?.enable === 'v1'}
                    onCheckedChange={() => handleToggle()}
                  />
                )}
              </div>
            </div>
          </>
        )}
      </div>
    </div>
  );
};
