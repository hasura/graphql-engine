import React from 'react';
import Toggle from 'react-toggle';
import { useServerConfig } from '@/hooks';
import KnowMoreLink from '@/components/Common/KnowMoreLink/KnowMoreLink';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

type ApolloFederationSupportProps = {
  toggleApollofederation: () => void;
  isApolloFederationSupported: boolean;
};

export const ApolloFederationSupport = ({
  toggleApollofederation,
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
    configData?.experimental_features.includes('apollo_federation');
  return (
    <div className="mb-lg">
      <div className="flex items-center mb-formlabel">
        <h4 className="flex items-center text-gray-600 font-semibold mr-sm">
          Enable Apollo Federation
          <ToolTip message="Allows Apollo federated subgraphs to use this table in their schema by adding the `@key` directive" />
        </h4>
        <KnowMoreLink
          href="https://hasura.io/docs/latest/data-federation/apollo-federation/"
          text="Know More"
        />
      </div>
      {!isSupportForApolloFederationEnabled ? (
        <div className="font-thin">
          Apollo federation is not enabled. To enable apollo federation support,
          start the Hasura server with environment variable
          <code>
            HASURA_GRAPHQL_EXPERIMENTAL_FEATURES: &quot;apollo_federation&quot;
          </code>
        </div>
      ) : (
        <div data-toggle="tooltip">
          <Toggle
            icons={false}
            onChange={() => toggleApollofederation()}
            checked={isApolloFederationSupported}
          />
        </div>
      )}
    </div>
  );
};
