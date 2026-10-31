import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { matchPath, useLocation } from 'react-router';
import { useMetadata } from '@hasura/metadata/api';
import DataSubSidebar from './DataSubSidebar';
import { useAppContext } from '@hasura/shared/context';
import { LeftSidebar } from '@hasura/shared/ui';

// The routes of each sidebar item, as declared in `features/Data/routes.tsx`.
// `/*` also matches the route itself, e.g. `/data/manage`.
export const ROUTE_PATTERNS = {
  manage: [
    '/data/manage/*',
    '/data/edit',
    '/data/schema/manage',
    // legacy routes, redirected to `/data/manage`
    '/data/v2/manage/*',
    '/data/:source/schema/:schema/*',
  ],
  sql: ['/data/sql'],
  nativeQueries: ['/data/native-queries/*'],
  modelSummary: ['/data/model-count-summary'],
  migrations: ['/data/migrations'],
};

export const matchesAny = (patterns: string[], pathname: string) =>
  patterns.some((pattern) => matchPath(pattern, pathname) !== null);

const LegacyDataSidebar = () => {
  const { pathname } = useLocation();
  const { data: meta, isLoading: metadataLoading } = useMetadata();
  const { envVars } = useAppContext();

  const isActive = (patterns: string[]) => matchesAny(patterns, pathname);
  const areSourcesPresent = Boolean(meta?.metadata?.sources?.length);

  return (
    <Analytics name="DataPageContainerSidebar" {...REDACT_EVERYTHING}>
      <LeftSidebar
        items={[
          {
            isActive: isActive(ROUTE_PATTERNS.manage),
            label: 'Data Manager',
            to: `/data/manage`,
            children: (
              <DataSubSidebar
                metadata={meta?.metadata}
                metadataLoading={metadataLoading}
              />
            ),
          },
          ...(areSourcesPresent
            ? [
                {
                  isActive: isActive(ROUTE_PATTERNS.sql),
                  label: 'SQL',
                  to: `/data/sql`,
                },
              ]
            : []),
          {
            isActive: isActive(ROUTE_PATTERNS.nativeQueries),
            label: 'Native Queries',
            to: `/data/native-queries`,
          },
          {
            isActive: isActive(ROUTE_PATTERNS.modelSummary),
            label: 'Model Summary',
            to: `/data/model-count-summary`,
          },
          ...(envVars.consoleMode === 'cli'
            ? [
                {
                  isActive: isActive(ROUTE_PATTERNS.migrations),
                  label: 'Migrations',
                  to: '/data/migrations',
                },
              ]
            : []),
        ]}
      />
    </Analytics>
  );
};

export default LegacyDataSidebar;
