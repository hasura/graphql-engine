// GraphiQL 5 uses the Monaco editor, which needs its web workers wired up before
// any editor is created, hence this top import. GraphiQL's official webpack
// setup spawns them as classic workers; the console's webpack build strips the
// `import.meta` token from worker chunks (MonacoWorkerPublicPathPlugin) so those
// classic workers parse and run — see that plugin for the full rationale.
import 'graphiql/setup-workers/webpack';
import { useMemo, useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { GraphiQL } from 'graphiql';
import { FaCheckCircle, FaExclamationTriangle } from 'react-icons/fa';
import GraphiQLErrorBoundary from './GraphiQLErrorBoundary';
import { getGraphQLEndpoint, getGraphQLWebSocketEndpoint } from '../../utils';
import { ResponseTimeWarning } from './ResponseTimeWarning';
import GraphiQLToolbarButtons from './GraphiQLToolbarButtons';
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import { useGraphqlWsClient } from './graphiqlLifecycle';
import { ExternalQuerySync } from './ExternalQuerySync';
import { GraphiQLThemeSync } from './GraphiQLThemeSync';
import { codeExplorer, explorer } from './plugins';
import { GraphiQLPlugin } from '@graphiql/react';
import {
  IconTooltip,
  Text,
  Tooltip,
  useOptionalAppearance,
} from '@hasura/shared/ui';
import { getCacheRequestWarning } from './utils';

import './GraphiQL.css';

type Props = {
  query: string;
  setQuery: (value: string) => void;
  mode: 'graphql' | 'relay';
  headers: Record<string, string>;
  visiblePlugin: GraphiQLPlugin | null;
  setVisiblePlugin: (plugin: GraphiQLPlugin | null) => void;
};

type ResponseMetrics = {
  responseTime: number;
  responseSize: number;
  isResponseCached: boolean;
  cacheWarning: string | null;
  isRequestCachable: boolean;
};

const MIN_LATENCY_FOR_CACHE = 5000;

const GraphiQLWrapper = ({
  mode,
  headers,
  query,
  setQuery,
  visiblePlugin,
  setVisiblePlugin,
}: Props) => {
  const [responseMetrics, setResponseMetrics] =
    useState<ResponseMetrics | null>();

  // Seed GraphiQL's first render with the current console appearance;
  // GraphiQLThemeSync (below) keeps it in sync on later toggles.
  const appearance = useOptionalAppearance()?.appearance ?? 'light';

  // graphql-ws subscription client, disposed on (mode/headers) change + unmount.
  // `null` on the first render (before its effect runs); until then the fetcher
  // is built without a ws client (HTTP works, subscriptions start once ready)
  // and is rebuilt when the client arrives via the `wsClient` dependency.
  const wsClient = useGraphqlWsClient(
    getGraphQLWebSocketEndpoint(mode),
    headers,
  );

  const fetcher = useMemo(
    () =>
      createGraphiQLFetcher({
        url: getGraphQLEndpoint(mode),
        headers,
        wsClient: wsClient ?? undefined,
        fetch: async (input: string | URL | Request, init?: RequestInit) => {
          setResponseMetrics(null);

          const startTime = new Date().getTime();
          const resp = await fetch(input, init);

          // Skip reporting query introspection.
          if (
            !init?.body ||
            typeof init.body !== 'string' ||
            init.body.includes('IntrospectionQuery')
          ) {
            return resp;
          }

          const endTime = new Date().getTime();
          const responseTime = endTime - startTime;
          const rawResponseSize = resp.headers.get('content-length');
          const responseSize = rawResponseSize
            ? parseInt(rawResponseSize)
            : (await resp.clone().bytes()).length;

          const isResponseCached = resp.headers.has('Cache-Control');
          const trimmedQuery = query.trim();
          const isRequestCachable =
            trimmedQuery.startsWith('{') || trimmedQuery.startsWith('query ');
          const cacheWarning = getCacheRequestWarning(
            resp.headers.get('Warning'),
          );

          const responseMetrics = {
            responseTime,
            responseSize,
            isResponseCached,
            cacheWarning,
            isRequestCachable,
          };

          setResponseMetrics(responseMetrics);
          return resp;
        },
      }),
    [mode, headers, wsClient],
  );

  const renderGraphiqlFooter = responseMetrics?.responseTime && (
    <GraphiQL.Footer>
      <Flex align="center" gap="2" className="py-2 px-1">
        <Text weight="medium" size="1" className="uppercase">
          Response Time
        </Text>
        <Text>{responseMetrics.responseTime} ms</Text>
        {responseMetrics.responseTime > MIN_LATENCY_FOR_CACHE && (
          <ResponseTimeWarning />
        )}
        {responseMetrics.responseSize && (
          <>
            <Text weight="medium" size="1" className="uppercase">
              Response Size
            </Text>
            <Text>{responseMetrics.responseSize} bytes</Text>
          </>
        )}
        {responseMetrics.isResponseCached && (
          <>
            <Text weight="medium" size="1" className="uppercase">
              Cached
            </Text>
            <IconTooltip
              message="This query response was cached using the @cached directive"
              side="top"
            />
            <Text color="green">
              <FaCheckCircle />
            </Text>
          </>
        )}
        {!responseMetrics.isResponseCached && responseMetrics.cacheWarning && (
          <>
            <Text weight="medium" size="1" className="uppercase">
              Not Cached
            </Text>
            <Tooltip
              content={`Response not cached due to: "${responseMetrics.cacheWarning}"`}
              side="top"
            >
              <Text color="amber">
                <FaExclamationTriangle />
              </Text>
            </Tooltip>
          </>
        )}
      </Flex>
    </GraphiQL.Footer>
  );

  return (
    <GraphiQLErrorBoundary>
      <div className="w-full h-full border overflow-hidden rounded border-gray-300 dark:border-slate-700">
        <GraphiQL
          fetcher={fetcher}
          initialQuery={query}
          onEditQuery={(q) => setQuery(q ?? '')}
          defaultTheme={appearance}
          plugins={[codeExplorer, explorer]}
          isHeadersEditorEnabled={false}
          visiblePlugin={visiblePlugin ?? undefined}
          onTogglePluginVisibility={setVisiblePlugin}
        >
          <ExternalQuerySync externalQuery={query} />
          <GraphiQLThemeSync />
          <GraphiQL.Toolbar>
            {({ prettify }) => (
              <>
                {prettify}
                <GraphiQLToolbarButtons mode={mode} headers={headers} />
              </>
            )}
          </GraphiQL.Toolbar>
          {renderGraphiqlFooter}
        </GraphiQL>
      </div>
    </GraphiQLErrorBoundary>
  );
};

export default GraphiQLWrapper;
