import { useNavigate, useParams } from 'react-router';
import { Flex } from '@radix-ui/themes';
import { IndicatorCard, Tabs, Badge } from '@hasura/shared/ui';
import { NativeQuery } from '@hasura/shared/types';
import { MetadataWrapper } from '../../components';
import { NativeQueryRelationships } from '../NativeQueryRelationships/NativeQueryRelationships';
import { RouteWrapper } from '../components/RouteWrapper';
import { injectRouteDetails } from '../components/route-wrapper-utils';
import { Routes } from '../constants';
import { NativeQueryTabs } from '../types';
import { AddNativeQuery } from './AddNativeQuery';
import { MetadataSelectors } from '@hasura/metadata/helpers';

export const NativeQueryRoute = () => {
  const params = useParams<{
    source: string;
    name: string;
    tabName?: NativeQueryTabs;
  }>();
  const { source, name, tabName } = params;

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
        <NativeQueryLandingPage
          source={source}
          name={name}
          tabName={tabName}
          nativeQuery={nativeQuery}
        />
      )}
    />
  );
};

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
  const push = useNavigate();

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
        onValueChange={(tab) =>
          push(
            injectRouteDetails(Routes.EditNativeQuery, {
              itemName: nativeQuery.root_field_name,
              itemSourceName: source,
              itemTabName: tab,
            }),
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
              <Flex align="center" gap="2" data-testid="untracked-tab">
                Relationships
                <Badge className={`px-xs`} color="gray">
                  {relationshipsCount}
                </Badge>
              </Flex>
            ),
            value: 'relationships',
          },
        ]}
      />
    </RouteWrapper>
  );
};
