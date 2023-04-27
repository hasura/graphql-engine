import { useEffect } from 'react';
import { useQuery, useQueryClient } from 'react-query';
import { LocalRelationship } from '../../../types';
import { getDriverPrefix, runMetadataQuery } from '../../../../DataSource';
import { MetadataSelectors } from '../../../../hasura-metadata-api';
import { useMetadata } from '../../../../hasura-metadata-api/useMetadata';
import { useHttpClient } from '../../../../Network';
import {
  addConstraintName,
  SuggestedRelationshipsResponse,
} from './useSuggestedRelationships';
import { useMetadataMigration } from '../../../../MetadataAPI';
import {
  BulkKeepGoingResponse,
  NamingConvention,
  Table,
} from '../../../../hasura-metadata-types';
import { getTrackedRelationshipsCacheKey } from '../../../../Data/TrackResources/components/hooks/useTrackedRelationships';
import { hasuraToast } from '../../../../../new-components/Toasts';

export type AddSuggestedRelationship = {
  name: string;
  columnNames: string[];
  relationshipType: 'object' | 'array';
  toTable?: Table;
  fromTable?: Table;
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

  const metadataMutation = useMetadataMigration<BulkKeepGoingResponse>({});
  const queryClient = useQueryClient();

  const onAddMultipleSuggestedRelationships = async (
    relationships: AddSuggestedRelationship[]
  ) => {
    const queries = relationships.map(relationship => {
      return {
        type: `${dataSourcePrefix}_create_${relationship.relationshipType}_relationship`,
        args: {
          table: relationship.fromTable,
          name: relationship.name,
          source: dataSourceName,
          using: {
            foreign_key_constraint_on:
              relationship.relationshipType === 'object'
                ? relationship.columnNames
                : {
                    table: relationship.toTable,
                    columns: relationship.columnNames,
                  },
          },
        },
      };
    });

    await metadataMutation.mutateAsync(
      {
        query: {
          type: 'bulk_keep_going',
          args: queries,
        },
      },
      {
        onSuccess: response => {
          response.forEach(result => {
            if ('error' in result) {
              hasuraToast({
                type: 'error',
                title: 'Error while tracking foreign key',
                children: result.error,
              });
            }
          });

          const successfullyTrackedCounter = response.filter(
            result => 'message' in result && result.message === 'success'
          ).length;
          const plural = successfullyTrackedCounter > 1 ? 's' : '';

          hasuraToast({
            type: 'success',
            title: 'Successfully tracked',
            message: `${successfullyTrackedCounter} foreign key${plural} tracked`,
          });
        },
        onSettled: () => {
          queryClient.invalidateQueries({
            queryKey: getAllSuggestedRelationshipsCacheQuery(
              dataSourceName,
              omitTracked
            ),
          });

          queryClient.invalidateQueries({
            queryKey: getTrackedRelationshipsCacheKey(dataSourceName),
          });
        },
      }
    );
  };

  const relationshipsWithConstraintName = addConstraintName(
    rawSuggestedRelationships,
    namingConvention
  );

  return {
    suggestedRelationships: relationshipsWithConstraintName,
    isLoadingSuggestedRelationships: isLoadingAllSuggestedRelationships,
    isFetchingSuggestedRelationships: isFetchingAllSuggestedRelationships,
    refetchSuggestedRelationships: refetchAllSuggestedRelationships,
    onAddMultipleSuggestedRelationships,
    ...rest,
  };
};
