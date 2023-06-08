import { InjectedRouter, withRouter } from 'react-router';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { MetadataUtils, useMetadata } from '../../../hasura-metadata-api';
import { RouteWrapper } from '../components/RouteWrapper';
import { AddNativeQuery } from './AddNativeQuery';
import { normalizeArguments } from './utils';
import Skeleton from 'react-loading-skeleton';

export const UpdateNativeQueryRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
  params: { source: string; name: string };
}>(({ location, router, params: { source, name } }) => {
  if (!source || !name) {
    return (
      <IndicatorCard status="negative">
        Unable to parse data from url.
      </IndicatorCard>
    );
  }

  const { data: sourceQueries, isLoading } = useMetadata(
    m => MetadataUtils.findMetadataSource(source, m)?.native_queries
  );

  const nativeQuery = sourceQueries?.find(s => s.root_field_name === name);

  if (!nativeQuery) {
    return (
      <IndicatorCard status="negative">
        Native Query {name} not found in {source}
      </IndicatorCard>
    );
  }

  return (
    <RouteWrapper
      route={'/data/native-queries/{{source}}/{{name}}'}
      itemSourceName={source}
      itemName={name}
    >
      {isLoading ? (
        <div>
          <Skeleton count={10} />
        </div>
      ) : (
        <AddNativeQuery
          mode={'update'}
          defaultFormValues={{
            ...nativeQuery,
            source: source,
            arguments: normalizeArguments(nativeQuery.arguments ?? {}),
          }}
        />
      )}
    </RouteWrapper>
  );
});
