import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useLocation } from 'react-router';
import { useMetadata } from '@hasura/metadata/api';
import DataSubSidebar from './DataSubSidebar';
import { useAppContext } from '@hasura/shared/context';
import { LeftSidebar } from '@hasura/shared/ui';

const MANAGE_DATA_REGEX =
  /(\/)?data((\/manage)|(\/(\w+)\/)|(\/(\w|%)+\/schema?(\w+)))/;

const LegacyDataSidebar = () => {
  const location = useLocation();
  const { data: meta, isFetching: metadataLoading } = useMetadata();
  const { envVars } = useAppContext();

  const currentLocation = location.pathname;
  const areSourcesPresent = Boolean(meta?.metadata?.sources?.length);

  return (
    <Analytics name="DataPageContainerSidebar" {...REDACT_EVERYTHING}>
      <LeftSidebar
        items={[
          {
            isActive: MANAGE_DATA_REGEX.test(currentLocation),
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
                  isActive: currentLocation.includes('/sql'),
                  label: 'SQL',
                  to: `/data/sql`,
                },
              ]
            : []),
          {
            isActive:
              currentLocation.includes('/native-queries') ||
              currentLocation.includes('/logical-models'),
            label: 'Native Queries',
            to: `/data/native-queries`,
          },
          {
            isActive: currentLocation.includes('/model-count-summary'),
            label: 'Model Summary',
            to: `/data/model-count-summary`,
          },
          ...(envVars.consoleMode === 'cli'
            ? [
                {
                  isActive: currentLocation.includes('data/migrations'),
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
