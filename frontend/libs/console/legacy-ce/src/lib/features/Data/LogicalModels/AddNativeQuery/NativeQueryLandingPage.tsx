import { withRouter } from 'react-router';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { Tabs } from '../../../../new-components/Tabs';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks';
import { MetadataSelectors } from '../../../hasura-metadata-api';
import { NativeQuery } from '../../../hasura-metadata-types';
import { MetadataWrapper } from '../../components';
import { NativeQueryRelationships } from '../NativeQueryRelationships/NativeQueryRelationships';
import { RouteWrapper } from '../components/RouteWrapper';
import { injectRouteDetails } from '../components/route-wrapper-utils';
import { Routes } from '../constants';
import { NativeQueryTabs } from '../types';
import { AddNativeQuery } from './AddNativeQuery';
import { Badge } from '../../../../new-components/Badge';

export const NativeQueryRoute = withRouter<{
  params: { source: string; name: string; tabName?: NativeQueryTabs };
}>(({ params }) => {
  const { source, name } = params;

  if (!source || !name) {
    return (
      <IndicatorCard status="negative">
        Unable to parse data from URL.
      </IndicatorCard>
    );
  }

  return (
    // bind metadata to UI component from URL Params:
    <MetadataWrapper
      selector={MetadataSelectors.findNativeQuery(source, name)}
      render={({ data: nativeQuery }) => (
        <NativeQueryLandingPage {...params} nativeQuery={nativeQuery} />
      )}
    />
  );
});

// presentational component that has no data fetching:
const NativeQueryLandingPage = ({
  name: nativeQueryName,
  source,
  tabName,
  nativeQuery,
}: {
  name: string;
  source: string;
  tabName?: string;
  nativeQuery: NativeQuery | undefined;
}) => {
  const push = usePushRoute();

  if (!nativeQuery) {
    return (
      <IndicatorCard status="negative">
        Native Query {nativeQueryName} not found in {source}
      </IndicatorCard>
    );
  }

  const relationshipsCount =
    (nativeQuery.array_relationships?.length ?? 0) +
    (nativeQuery.object_relationships?.length ?? 0);

  return (
    <RouteWrapper
      route={Routes.EditNativeQuery}
      itemSourceName={source}
      itemName={nativeQuery?.root_field_name}
      itemTabName={tabName}
      subtitle={
        tabName === 'details'
          ? 'Make changes to your Native Query'
          : 'Add/Remove relationships to your Native Query'
      }
    >
      <Tabs
        value={tabName ?? 'details'}
        onValueChange={tab =>
          push(
            injectRouteDetails(Routes.EditNativeQuery, {
              itemName: nativeQuery.root_field_name,
              itemSourceName: source,
              itemTabName: tab,
            })
          )
        }
        items={[
          {
            content: (
              <AddNativeQuery
                editDetails={{ dataSourceName: source, nativeQuery }}
              />
            ),
            label: 'Details',
            value: 'details',
          },
          {
            content: (
              <NativeQueryRelationships
                dataSourceName={source}
                nativeQueryName={nativeQueryName}
              />
            ),
            label: (
              <div
                className="flex items-center gap-2"
                data-testid="untracked-tab"
              >
                Relationships
                <Badge className={`px-xs`} color="dark-gray">
                  {relationshipsCount}
                </Badge>
              </div>
            ),
            value: 'relationships',
          },
        ]}
      />
    </RouteWrapper>
  );
};
