import { InjectedRouter, withRouter } from 'react-router';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { Tabs } from '../../../../new-components/Tabs';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import { NativeQueryRelationships } from '../NativeQueryRelationships/NativeQueryRelationships';
import { RouteWrapper } from '../components/RouteWrapper';
import { injectRouteDetails } from '../components/route-wrapper-utils';
import { NATIVE_QUERY_ROUTES } from '../constants';
import { AddNativeQuery } from './AddNativeQuery';

type allowedTabs = 'details' | 'relationships';

export type NativeQueryLandingPage = {
  params: { source: string; name: string; tabName?: allowedTabs };
  location: Location;
  router: InjectedRouter;
};

export const NativeQueryLandingPage = withRouter<{
  location: Location;
  router: InjectedRouter;
  params: { source: string; name: string; tabName?: allowedTabs };
}>(props => {
  const {
    params: { source: dataSourceName, name: nativeQueryName, tabName },
  } = props;

  const { data: nativeQuery } = useMetadata(
    MetadataSelectors.findNativeQuery(dataSourceName, nativeQueryName)
  );

  const push = usePushRoute();

  const route: keyof typeof NATIVE_QUERY_ROUTES =
    '/data/native-queries/{{source}}/{{name}}/{{tab}}';

  if (!nativeQuery) {
    return (
      <IndicatorCard status="negative">
        Native Query {nativeQueryName} not found in {dataSourceName}
      </IndicatorCard>
    );
  }

  const relationshipsCount =
    (nativeQuery.array_relationships?.length ?? 0) +
    (nativeQuery.object_relationships?.length ?? 0);

  return (
    <RouteWrapper
      route={route}
      itemSourceName={dataSourceName}
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
            injectRouteDetails(route, {
              itemName: nativeQuery.root_field_name,
              itemSourceName: dataSourceName,
              itemTabName: tab,
            })
          )
        }
        items={[
          {
            content: (
              <AddNativeQuery editDetails={{ dataSourceName, nativeQuery }} />
            ),
            label: 'Details',
            value: 'details',
          },
          {
            content: (
              <NativeQueryRelationships
                dataSourceName={dataSourceName}
                nativeQueryName={nativeQueryName}
              />
            ),
            label: (
              <div className="flex items-center" data-testid="untracked-tab">
                Relationships
                <span className="bg-gray-300 ml-1 px-1.5 py-0.5 rounded text-xs">
                  {relationshipsCount}
                </span>
              </div>
            ),
            value: 'relationships',
          },
        ]}
      />
    </RouteWrapper>
  );
});
