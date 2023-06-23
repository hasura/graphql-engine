import { useEffect } from 'react';
import { useQuery } from 'react-query';
import { LocalRelationship } from '../../../types';
import { getDriverPrefix, runMetadataQuery } from '../../../../DataSource';
import { MetadataSelectors } from '../../../../hasura-metadata-api';
import { useMetadata } from '../../../../hasura-metadata-api/useMetadata';
import { useHttpClient } from '../../../../Network';
import {
  addConstraintName,
  SuggestedRelationshipsResponse,
} from './useSuggestedRelationships';
import { NamingConvention, Table } from '../../../../hasura-metadata-types';

export type AddSuggestedRelationship = {
  name: string;
  fromColumnNames: string[];
  toColumnNames: string[];
  relationshipType: 'object' | 'array';
  toTable?: Table;
  fromTable?: Table;
  constraintOn: 'fromTable' | 'toTable';
};

type UseSuggestedRelationshipsArgs = {
  dataSourceName: string;
  existingRelationships?: LocalRelationship[];
  isEnabled: boolean;
  omitTracked: boolean;
};

export const getAllSuggestedRelationshipsCacheQuery = (
  dataSourceName: string,
  omitTracked: boolean
) => ['all_suggested_relationships', dataSourceName, omitTracked];

export const useAllSuggestedRelationships = ({
  dataSourceName,
  isEnabled,
  omitTracked,
}: UseSuggestedRelationshipsArgs) => {
  const { data: metadataSource, isFetching } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const dataSourcePrefix = metadataSource?.kind
    ? getDriverPrefix(metadataSource?.kind)
    : undefined;

  const namingConvention: NamingConvention =
    metadataSource?.customization?.naming_convention || 'hasura-default';

  const httpClient = useHttpClient();

  const {
    data,
    refetch: refetchAllSuggestedRelationships,
    isLoading: isLoadingAllSuggestedRelationships,
    isFetching: isFetchingAllSuggestedRelationships,
    ...rest
  } = useQuery({
    queryKey: getAllSuggestedRelationshipsCacheQuery(
      dataSourceName,
      omitTracked
    ),
    queryFn: async () => {
      const body = {
        type: `${dataSourcePrefix}_suggest_relationships`,
        args: {
          omit_tracked: omitTracked,
          source: dataSourceName,
        },
      };
      const result = await runMetadataQuery<SuggestedRelationshipsResponse>({
        httpClient,
        body,
      });
      return result;
    },
    enabled: isEnabled && !isFetching,
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (dataSourcePrefix) {
      refetchAllSuggestedRelationships();
    }
  }, [dataSourcePrefix]);

  const rawSuggestedRelationships = data?.relationships || [];

  const relationshipsWithConstraintName = addConstraintName(
    rawSuggestedRelationships,
    namingConvention
  );

  return {
    suggestedRelationships: relationshipsWithConstraintName,
    isLoadingSuggestedRelationships: isLoadingAllSuggestedRelationships,
    isFetchingSuggestedRelationships: isFetchingAllSuggestedRelationships,
    refetchSuggestedRelationships: refetchAllSuggestedRelationships,
    ...rest,
  };
};
