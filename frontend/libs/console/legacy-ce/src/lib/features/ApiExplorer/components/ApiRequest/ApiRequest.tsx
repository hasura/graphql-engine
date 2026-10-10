import { useState } from 'react';
import { Flex, Skeleton } from '@radix-ui/themes';
import { IconTooltip, Input, Switch } from '@hasura/shared/ui';
import GraphiQLWrapper from '../GraphiQLWrapper/GraphiQLWrapper';
import { getGraphQLEndpoint } from '../../utils';
import useApiExplorer from './useApiExplorer';
import HeaderTable from './HeaderTable';
import { GraphiQLPlugin } from '@graphiql/react';
import { explorer } from '../GraphiQLWrapper/plugins';
import TokenAnalyzeDialog from './TokenAnalyzeDialog';
import { useServerConfig } from '@hasura/metadata/api';

/* When the page is loaded for the first time, hydrate the header state from the localStorage
 * Keep syncing the localStorage state when user modifies.
 * */

type Props = {
  numberOfTables: number;
};

const ApiRequest = ({ numberOfTables }: Props) => {
  const { data: serverConfig } = useServerConfig();
  const [visiblePlugin, setVisiblePlugin] = useState<GraphiQLPlugin | null>(
    explorer,
  );

  const {
    query,
    setQuery,
    toggleGraphiqlMode,
    mode,
    headers,
    objectHeaders,
    handleHeaderFocus,
    handleHeaderUnfocus,
    removeRequestHeader,
    changeRequestHeader,
    analyzingToken,
    tokenInfo,
    analyzeBearerToken,
    resetAnalyzingToken,
    headersInitialized,
  } = useApiExplorer({
    numberOfTables,
  });

  const getGraphQLEndpointBar = () => {
    return (
      <Flex justify="between" gap="2">
        <Input
          value={getGraphQLEndpoint(mode)}
          type="text"
          readOnly
          containerClassName="w-full"
          prependLabel="POST"
        />

        <Flex
          align="center"
          className="ml-4 cursor-pointer w-[160px]"
          gap="2"
          onClick={toggleGraphiqlMode}
        >
          <Switch
            value={mode === 'relay'}
            className="flex"
            disabled={!headersInitialized}
          >
            Relay API
          </Switch>
          <IconTooltip
            side="left"
            message={
              'Toggle to point this GraphiQL to a relay-compliant GraphQL API served at /v1/relay'
            }
          />
        </Flex>
      </Flex>
    );
  };

  const getRequestBody = () => {
    if (!headersInitialized) {
      return <Skeleton height="500px" width="100%" />;
    }

    return (
      <div
        className={
          'h-[calc(100vh-450px)] min-h-[500px] my-4 resize-y overflow-auto'
        }
      >
        <GraphiQLWrapper
          mode={mode}
          query={query}
          setQuery={setQuery}
          headers={objectHeaders}
          visiblePlugin={visiblePlugin}
          setVisiblePlugin={setVisiblePlugin}
        />
      </div>
    );
  };

  return (
    <Flex className="py-4" direction="column" gap="4">
      {getGraphQLEndpointBar()}
      <HeaderTable
        analyzeBearerToken={analyzeBearerToken}
        analyzingToken={analyzingToken}
        changeRequestHeader={changeRequestHeader}
        headers={headers}
        removeRequestHeader={removeRequestHeader}
        serverConfig={serverConfig}
        handleHeaderFocus={handleHeaderFocus}
        handleHeaderUnfocus={handleHeaderUnfocus}
      />
      {getRequestBody()}
      {analyzingToken.isAnalyzing && (
        <TokenAnalyzeDialog
          resetAnalyzingToken={resetAnalyzingToken}
          serverConfig={serverConfig}
          tokenInfo={tokenInfo}
        />
      )}
    </Flex>
  );
};

export default ApiRequest;
