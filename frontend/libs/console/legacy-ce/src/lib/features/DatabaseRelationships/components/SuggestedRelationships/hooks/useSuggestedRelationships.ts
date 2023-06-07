import { useEffect, useState } from 'react';
import inflection from 'inflection';
import camelCase from 'lodash/camelCase';
import { LocalRelationship, SuggestedRelationship } from '../../../types';
import { getTableDisplayName } from '../../../utils/helpers';
import { getDriverPrefix, runMetadataQuery } from '../../../../DataSource';
import {
  areTablesEqual,
  MetadataSelectors,
} from '../../../../hasura-metadata-api';
import { useMetadata } from '../../../../hasura-metadata-api/useMetadata';
import { NamingConvention, Table } from '../../../../hasura-metadata-types';
import { useHttpClient } from '../../../../Network';
import { useQuery, useQueryClient } from 'react-query';
import { generateQueryKeys } from '../../../utils/queryClientUtils';
import { useMetadataMigration } from '../../../../MetadataAPI';
import { useDriverRelationshipSupport } from '../../../../Data/hooks/useDriverRelationshipSupport';
import { hasuraToast } from '../../../../../new-components/Toasts/hasuraToast';
import adaptTrackRelationship from '../../../../Data/TrackResources/components/utils/adaptTrackRelationship';
import { getLocalRelationshipPayload } from '../adapters/getLocalRelationshipPayload';

type UseSuggestedRelationshipsArgs = {
  dataSourceName: string;
  table?: Table;
  existingRelationships?: LocalRelationship[];
  isEnabled: boolean;
};

export type SuggestedRelationshipsResponse = {
  relationships: SuggestedRelationship[];
};

type FilterTableRelationshipsArgs = {
  table: Table;
  relationships: SuggestedRelationship[];
};

export const filterTableRelationships = ({
  table,
  relationships,
}: FilterTableRelationshipsArgs) =>
  relationships.filter(relationship => {
    if (areTablesEqual(relationship.from.table, relationship.to.table)) {
      return false;
    }
    return areTablesEqual(relationship.from.table, table);
  });

export type SuggestedRelationshipWithName = SuggestedRelationship & {
  constraintName: string;
};

type GetRelationTableNameArg = {
  table: Table;
  relationshipType: SuggestedRelationship['type'];
};

const formatRelationToTableName = ({
  table,
  relationshipType,
}: GetRelationTableNameArg) => {
  const baseTableName = getTableDisplayName(table);
  if (relationshipType === 'array') {
    return inflection.pluralize(baseTableName);
  }

  return inflection.singularize(getTableDisplayName(table));
};

const makeStringGraphQLCompliant = (text: string) => text.replace(/\./g, '_');

export const addConstraintName = (
  relationships: SuggestedRelationship[],
  namingConvention: NamingConvention
): SuggestedRelationshipWithName[] =>
  relationships.map(relationship => {
    const fromTable = getTableDisplayName(relationship.from.table);
    const toTableName = formatRelationToTableName({
      table: relationship.to.table,
      relationshipType: relationship.type,
    });

    const baseConstraintName = makeStringGraphQLCompliant(
      `${fromTable}_${toTableName}`
    );

    const constraintName =
      namingConvention === 'graphql-default'
        ? camelCase(baseConstraintName)
        : baseConstraintName;

    return {
      ...relationship,
      constraintName,
    };
  });

export const getSuggestedRelationshipsCacheQuery = (
  dataSourceName: string,
  table: Table
) => ['suggested_relationships', dataSourceName, table];

export const useSuggestedRelationships = ({
  dataSourceName,
  table,
  existingRelationships = [],
  isEnabled,
}: UseSuggestedRelationshipsArgs) => {
  const { data: metadataSource, isFetching } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const { driverSupportsLocalRelationship, driverSupportsRemoteRelationship } =
    useDriverRelationshipSupport({
      dataSourceName,
    });

  const namingConvention: NamingConvention =
    metadataSource?.customization?.naming_convention || 'hasura-default';

  const metadataMutation = useMetadataMigration({});

  const queryClient = useQueryClient();

  const dataSourcePrefix = metadataSource?.kind
    ? getDriverPrefix(metadataSource?.kind)
    : undefined;

  const httpClient = useHttpClient();

  const {
    data,
    refetch: refetchSuggestedRelationships,
    isLoading: isLoadingSuggestedRelationships,
  } = useQuery({
    queryKey: getSuggestedRelationshipsCacheQuery(dataSourceName, table),
    queryFn: async () => {
      const body = {
        type: `${dataSourcePrefix}_suggest_relationships`,
        args: {
          omit_tracked: true,
          source: dataSourceName,
          ...(table ? { tables: [table] } : {}),
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

  const [isAddingSuggestedRelationship, setAddingSuggestedRelationship] =
    useState(false);

  const onAddSuggestedRelationship = async (
    relationship: SuggestedRelationshipWithName
  ) => {
    const addRelationship = adaptTrackRelationship(relationship);

    setAddingSuggestedRelationship(true);

    if (!driverSupportsLocalRelationship && !driverSupportsRemoteRelationship) {
      hasuraToast({
        type: 'error',
        title: 'Not able to track',
        message: `This datasource does not support tracking of relationships.`,
      });
      return;
    }

    if (driverSupportsLocalRelationship) {
      await metadataMutation.mutateAsync({
        query: getLocalRelationshipPayload({
          dataSourcePrefix: dataSourcePrefix || '',
          dataSourceName,
          relationship: addRelationship,
        }),
      });
    } else if (driverSupportsRemoteRelationship) {
      const {
        fromTable,
        name,
        relationshipType,
        fromColumnNames,
        toColumnNames,
      } = addRelationship;

      await metadataMutation.mutateAsync({
        query: {
          type: `${dataSourcePrefix}_create_remote_relationship`,
          args: {
            table: fromTable || table,
            name,
            source: dataSourceName,
            definition: {
              to_source: {
                relationship_type: relationshipType,
                source: dataSourceName,
                table: fromTable || table,
                field_mapping: fromColumnNames?.reduce((tally, curr, i) => {
                  return {
                    ...tally,
                    [curr]: toColumnNames[i],
                  };
                }, {}),
              },
            },
          },
        },
      });
    }

    hasuraToast({
      title: 'Success',
      message: 'Relationship tracked',
      type: 'success',
    });
    setAddingSuggestedRelationship(false);

    queryClient.invalidateQueries({
      queryKey: generateQueryKeys.metadata(),
    });

    queryClient.invalidateQueries({
      queryKey: getSuggestedRelationshipsCacheQuery(dataSourceName, table),
    });
  };

  useEffect(() => {
    if (dataSourcePrefix) {
      refetchSuggestedRelationships();
    }
  }, [dataSourcePrefix, refetchSuggestedRelationships]);

  const suggestedRelationships = data?.relationships || [];

  /**
   * This is needed because the suggested_relationships metadata API returns Foreign Keys
   *
   * from current table -> to other table
   * but also
   *
   * from other table -> to current table
   *
   * After the tracking, the second type of Foreign Keys would not be shown in the current table UI
   */
  const tableFilteredRelationships = table
    ? filterTableRelationships({
        table,
        relationships: suggestedRelationships,
      })
    : suggestedRelationships;

  const relationshipsWithConstraintName = addConstraintName(
    tableFilteredRelationships,
    namingConvention
  );

  return {
    suggestedRelationships: relationshipsWithConstraintName,
    isLoadingSuggestedRelationships,
    refetchSuggestedRelationships,
    onAddSuggestedRelationship,
    isAddingSuggestedRelationship,
  };
};
