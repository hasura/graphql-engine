import { useEffect, useState } from 'react';
import inflection from 'inflection';
import camelCase from 'lodash.camelcase';
import { isEqual } from '../../../../../components/Common/utils/jsUtils';
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

type UseSuggestedRelationshipsArgs = {
  dataSourceName: string;
  table: Table;
  existingRelationships: LocalRelationship[];
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

type RemoveExistingRelationshipsArgs = {
  relationships: SuggestedRelationship[];
  existingRelationships: LocalRelationship[];
};

export const removeExistingRelationships = ({
  relationships,
  existingRelationships,
}: RemoveExistingRelationshipsArgs) =>
  relationships.filter(relationship => {
    const fromTable = relationship.from.table;

    const fromTableExists = existingRelationships.find(rel =>
      areTablesEqual(rel.fromTable, fromTable)
    );

    if (!fromTableExists) {
      return true;
    }

    const existingRelationshipsFromSameTable = existingRelationships.filter(
      rel => areTablesEqual(rel.fromTable, fromTable)
    );

    const toTable = relationship.to.table;
    const toTableExists = existingRelationshipsFromSameTable.find(rel =>
      areTablesEqual(rel.definition.toTable, toTable)
    );

    if (!toTableExists) {
      return true;
    }

    const existingRelationshipsFromAndToSameTable =
      existingRelationshipsFromSameTable.filter(rel =>
        areTablesEqual(rel.definition.toTable, toTable)
      );

    const existingRelationshipsFromAndToSameTableAndSameFromColumns =
      existingRelationshipsFromAndToSameTable.filter(rel => {
        const existingToColumns = Object.values(rel.definition.mapping).sort();
        const relationshipToColumns = relationship.to.columns.sort();

        return isEqual(existingToColumns, relationshipToColumns);
      });

    if (!existingRelationshipsFromAndToSameTableAndSameFromColumns) {
      return true;
    }

    return false;
  });

export const useSuggestedRelationships = ({
  dataSourceName,
  table,
  existingRelationships,
  isEnabled,
}: UseSuggestedRelationshipsArgs) => {
  const { data: metadataSource } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

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
    queryKey: ['suggested_relationships', dataSourceName, table],
    queryFn: async () => {
      const body = {
        type: `${dataSourcePrefix}_suggest_relationships`,
        args: {
          omit_tracked: true,
          tables: [table],
          source: dataSourceName,
        },
      };
      const result = await runMetadataQuery<SuggestedRelationshipsResponse>({
        httpClient,
        body,
      });

      return result;
    },
    enabled: isEnabled,
  });

  const [isAddingSuggestedRelationship, setAddingSuggestedRelationship] =
    useState(false);

  const onAddSuggestedRelationship = async ({
    name,
    columnNames,
    relationshipType,
    toTable,
  }: {
    name: string;
    columnNames: string[];
    relationshipType: 'object' | 'array';
    toTable?: Table;
  }) => {
    setAddingSuggestedRelationship(true);

    await metadataMutation.mutateAsync({
      query: {
        type: `${dataSourcePrefix}_create_${relationshipType}_relationship`,
        args: {
          table,
          name,
          source: dataSourceName,
          using: {
            foreign_key_constraint_on:
              relationshipType === 'object'
                ? columnNames
                : {
                    table: toTable,
                    columns: columnNames,
                  },
          },
        },
      },
    });
    setAddingSuggestedRelationship(false);

    queryClient.invalidateQueries({
      queryKey: generateQueryKeys.metadata(),
    });
  };

  useEffect(() => {
    if (dataSourcePrefix) {
      refetchSuggestedRelationships();
    }
  }, [dataSourcePrefix]);

  const suggestedRelationships = data?.relationships || [];

  const tableFilteredRelationships = filterTableRelationships({
    table,
    relationships: suggestedRelationships,
  });

  // TODO: remove when the metadata request will correctly omit already tracked relationships
  const notExistingRelationships = removeExistingRelationships({
    relationships: tableFilteredRelationships,
    existingRelationships,
  });

  const relationshipsWithConstraintName = addConstraintName(
    notExistingRelationships,
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
