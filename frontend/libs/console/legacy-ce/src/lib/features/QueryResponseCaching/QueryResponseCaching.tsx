import React from 'react';
import { EETrialCard, useEELiteAccess } from '../EETrial';
// import { StatusBadge } from './StatusBadge';
import { LearnMoreLink, Link, Text } from '@hasura/shared/ui';
import { QueryResponseCachingSvg } from './QueryResponseCachingSvg';
import { Code, Heading } from '@radix-ui/themes';
// import { StatusText } from './StatusText';

const Header = () => (
  <>
    <Heading size="4">Query Response Caching</Heading>
    <div className="my-2">
      Improve API performance by caching frequently executed GraphQL queries.
      <LearnMoreLink href="https://hasura.io/docs/latest/enterprise/caching/" />
    </div>
    <QueryResponseCachingSvg />
  </>
);

const Body = () => {
  return (
    <div className="mt-4">
      {/*<StatusText status="disabled" />*/}
      <div className="my-2">
        <Text weight="bold">Setup Query Caching</Text>{' '}
      </div>
      <div>
        <Link
          href="https://hasura.io/docs/latest/enterprise/caching/"
          target="_blank"
          rel="noopener noreferrer"
        >
          Read more
        </Link>{' '}
        <Text>on setting up caching for your Hasura GraphQL instance.</Text>
      </div>
      <Text as="p">
        Redis may be enabled by setting the environment variable:{' '}
        <Code>HASURA_GRAPHQL_REDIS_URL</Code>
      </Text>
      <Text as="p">
        GraphQL operations using the <Code>cache</Code> directive will be served
        using your cache.
      </Text>
    </div>
  );
};

export const QueryResponseCaching: React.FC<Record<string, never>> = () => {
  const eeLite = useEELiteAccess();

  const isFeatureForbidden =
    eeLite.consoleType === 'oss' ||
    (eeLite.consoleType === 'pro-lite' && eeLite.access === 'forbidden');

  if (isFeatureForbidden) return null;

  const isFeatureActive =
    eeLite.consoleType !== 'pro-lite' || eeLite.access === 'active';

  return (
    <div className="max-w-(--breakpoint-lg) p-4">
      <div className="max-w-3xl">
        <Header />
        {isFeatureActive ? (
          <Body />
        ) : (
          <EETrialCard
            className="mt-4"
            id="query-response-caching"
            cardTitle="Improve performance and save resources with query response caching"
            cardText={
              <span>
                By storing and quickly returning results for repeated queries,
                query caching can improve the performance of your application.
                This saves time and resources from your origin sources.
              </span>
            }
            buttonLabel="Enable Enterprise"
            eeAccess={eeLite.access}
            horizontal
          />
        )}
      </div>
    </div>
  );
};
