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
import { useDriverRelationshipSupport } from '../../../../Data/hooks/useDriverRelationshipSupport';

type QueriesType =
  | {
      type: string;
      args: {
        table: unknown;
        name: string;
        source: string;
        using: {
          foreign_key_constraint_on:
            | string[]
            | { table: unknown; columns: string[] };
        };
      };
    }[]
  | {
      type: string;
      args: {
        table: unknown;
        name: string;
        source: string;
        definition: {
          to_source: {
            relationship_type: 'object' | 'array';
            source: any;
            table: any;
            field_mapping: { [x: string]: string };
          };
        };
      };
    }[];

export type AddSuggestedRelationship = {
  name: string;
  fromColumnNames: string[];
  toColumnNames: string[];
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

  const { driverSupportsLocalRelationship, driverSupportsRemoteRelationship } =
    useDriverRelationshipSupport({
      dataSourceName,
    });

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
    let queries: QueriesType = [];

    if (!driverSupportsLocalRelationship && !driverSupportsRemoteRelationship) {
      hasuraToast({
        type: 'error',
        title: 'Not able to track',
        message: `This datasource does not support tracking of relationships.`,
      });
      return;
    }
    if (driverSupportsLocalRelationship) {
      queries = relationships.map(relationship => {
        return {
          type: `${dataSourcePrefix}_create_${relationship.relationshipType}_relationship`,
          args: {
            table: relationship.fromTable,
            name: relationship.name,
            source: dataSourceName,
            using: {
              foreign_key_constraint_on:
                relationship.relationshipType === 'object'
                  ? relationship.fromColumnNames
                  : {
                      table: relationship.toTable,
                      columns: relationship.toColumnNames,
                    },
            },
          },
        };
      });
    } else if (driverSupportsRemoteRelationship) {
      queries = relationships.map(relationship => {
        return {
          type: `${dataSourcePrefix}_create_remote_relationship`,
          args: {
            table: relationship.fromTable,
            name: relationship.name,
            source: dataSourceName,
            definition: {
              to_source: {
                relationship_type: relationship.relationshipType,
                source: dataSourceName,
                table: relationship.fromTable,
                field_mapping: relationship?.fromColumnNames?.reduce(
                  (tally, curr, i) => {
                    return {
                      ...tally,
                      [curr]: relationship.toColumnNames[i],
                    };
                  },
                  {}
                ),
              },
            },
          },
        };
      });
    }

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
