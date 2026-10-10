import React from 'react';
import { EETrialCard, useEELiteAccess } from '../EETrial';
// import { StatusBadge } from './StatusBadge';
import { LearnMoreLink } from '@hasura/shared/ui';
import { QueryResponseCachingSvg } from './QueryResponseCachingSvg';
import { Flex } from '@radix-ui/themes';
// import { StatusText } from './StatusText';

const Header = () => (
  <>
    <Flex align="center" gap="4" className="mb-1">
      <h1 className="text-xl font-semibold">Query Response Caching</h1>
    </Flex>
    <p className="text-muted mb-1">
      Improve API performance by caching frequently executed GraphQL queries.
      <LearnMoreLink href="https://hasura.io/docs/latest/enterprise/caching/" />
    </p>
    <QueryResponseCachingSvg />
  </>
);

const Body = () => {
  return (
    <div className="mt-4">
      {/*<StatusText status="disabled" />*/}
      <div className="font-semibold text-muted mt-2">
        Setup Query Caching
      </div>{' '}
      <p className="text-muted">
        <a
          href="https://hasura.io/docs/latest/enterprise/caching/"
          target="_blank"
          rel="noopener noreferrer"
          className="font-semibold text-[#337ab7]"
        >
          Read more
        </a>{' '}
        on setting up caching for your Hasura GraphQL instance.
      </p>
      <p className="text-muted">
        Redis may be enabled by setting the environment variable:{' '}
        <code>HASURA_GRAPHQL_REDIS_URL</code>
      </p>
      <p className="text-muted">
        GraphQL operations using the <code>cache</code> directive will be served
        using your cache.
      </p>
    </div>
  );
};

export const QueryResponseCaching: React.FC<Record<string, never>> = () => {
  const eeLite = useEELiteAccess();

  const isFeatureForbidden = eeLite.access === 'forbidden';

  const isFeatureActive = eeLite.access === 'active';

  if (isFeatureForbidden) return null;

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
