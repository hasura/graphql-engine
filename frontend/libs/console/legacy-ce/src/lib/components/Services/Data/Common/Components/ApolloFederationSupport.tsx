import React from 'react';
import Toggle from 'react-toggle';
import { useServerConfig } from '../../../../../hooks';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

export type ApolloFederationSupportProps = {
  toggleApolloFederation: () => void;
  isApolloFederationSupported: boolean;
};

export const ApolloFederationSupport = ({
  toggleApolloFederation,
  isApolloFederationSupported,
}: ApolloFederationSupportProps) => {
  const { data: configData, isLoading, isError } = useServerConfig();

  if (isError) {
    return <div>Error in fetching server configuration</div>;
  }

  if (isLoading) {
    return <div>Loading...</div>;
  }

  const isSupportForApolloFederationEnabled =
    configData?.is_apollo_federation_enabled;
  return (
    <div className="mb-lg">
      <div className="flex items-center mb-formlabel">
        <h4 className="flex items-center text-gray-600 font-semibold">
          Enable Apollo Federation
          <ToolTip message="Allows Apollo federated subgraphs to use this table in their schema by adding the `@key` directive" />
        </h4>
        <LearnMoreLink href="https://hasura.io/docs/latest/data-federation/apollo-federation/" />
      </div>
      {!isSupportForApolloFederationEnabled ? (
        <div className="font-thin">
          Apollo federation is not enabled. To enable apollo federation support,
          set the{' '}
          <a href="https://hasura.io/docs/latest/hasura-cloud/projects/env-vars/">
            project env variable
          </a>{' '}
          or start the Hasura server with{' '}
          <a href="https://hasura.io/docs/latest/deployment/graphql-engine-flags/index/#using-environment-variables">
            {' '}
            environment variable{' '}
          </a>
          <code>HASURA_GRAPHQL_ENABLE_APOLLO_FEDERATION: &quot;true&quot;</code>
        </div>
      ) : (
        <div data-toggle="tooltip">
          <Toggle
            icons={false}
            onChange={() => toggleApolloFederation()}
            checked={isApolloFederationSupported}
          />
        </div>
      )}
    </div>
  );
};
