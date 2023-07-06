import { useEffect } from 'react';
import inflection from 'inflection';
import camelCase from 'lodash/camelCase';
import { SuggestedRelationship } from '../../../types';
import { getTableDisplayName } from '../../../utils/helpers';
import { getDriverPrefix, runMetadataQuery } from '../../../../DataSource';
import {
  areTablesEqual,
  MetadataSelectors,
} from '../../../../hasura-metadata-api';
import { useMetadata } from '../../../../hasura-metadata-api/useMetadata';
import { NamingConvention, Table } from '../../../../hasura-metadata-types';
import { useHttpClient } from '../../../../Network';
import { useQuery } from 'react-query';

type UseSuggestedRelationshipsArgs = {
  dataSourceName: string;
  table?: Table;
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
  isEnabled,
}: UseSuggestedRelationshipsArgs) => {
  const { data: metadataSource, isFetching } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const namingConvention: NamingConvention =
    metadataSource?.customization?.naming_convention || 'hasura-default';

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
  };
};
