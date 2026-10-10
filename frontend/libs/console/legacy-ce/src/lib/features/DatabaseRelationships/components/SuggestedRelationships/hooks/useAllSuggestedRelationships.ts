import { useQuery } from '@tanstack/react-query';
import { LocalRelationship } from '../../../types';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { runMetadataQuery } from '@hasura/metadata/api';
import {
  addConstraintName,
  SuggestedRelationshipsResponse,
} from './useSuggestedRelationships';
import { NamingConvention, Source, Table } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';

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
  source: Source;
  existingRelationships?: LocalRelationship[];
  isEnabled: boolean;
  omitTracked: boolean;
};

export const getAllSuggestedRelationshipsCacheQuery = (
  dataSourceName: string,
  omitTracked: boolean,
) => [dataSourceName, 'all_suggested_relationships', omitTracked];

export const useAllSuggestedRelationships = ({
  source,
  isEnabled,
  omitTracked,
}: UseSuggestedRelationshipsArgs) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const {
    data,
    refetch: refetchAllSuggestedRelationships,
    isLoading: isLoadingAllSuggestedRelationships,
    isFetching: isFetchingAllSuggestedRelationships,
    ...rest
  } = useQuery({
    queryKey: getAllSuggestedRelationshipsCacheQuery(source.name, omitTracked),
    queryFn: async () => {
      const dataSourcePrefix = getDriverPrefix(source.kind);

      const body = {
        type: `${dataSourcePrefix}_suggest_relationships` as const,
        args: {
          omit_tracked: omitTracked,
          source: source.name,
        },
      };
      const result = await runMetadataQuery<SuggestedRelationshipsResponse>({
        url: endpoints.metadata,
        fetchJson,
        body,
      });
      return result;
    },
    enabled: isEnabled,
    refetchOnWindowFocus: false,
  });

  const namingConvention: NamingConvention =
    source.customization?.naming_convention || 'hasura-default';

  const rawSuggestedRelationships = data?.relationships || [];

  const relationshipsWithConstraintName = addConstraintName(
    rawSuggestedRelationships,
    namingConvention,
  );

  return {
    suggestedRelationships: relationshipsWithConstraintName,
    isLoadingSuggestedRelationships: isLoadingAllSuggestedRelationships,
    isFetchingSuggestedRelationships: isFetchingAllSuggestedRelationships,
    refetchSuggestedRelationships: refetchAllSuggestedRelationships,
    ...rest,
  };
};
