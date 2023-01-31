import inflection from 'inflection';
import { isEqual } from '@/components/Common/utils/jsUtils';
import {
  LocalRelationship,
  SuggestedRelationship,
} from '@/features/DatabaseRelationships/types';
import { getTableDisplayName } from '@/features/DatabaseRelationships/utils/helpers';
import { getDriverPrefix, NetworkArgs } from '@/features/DataSource';
import {
  areTablesEqual,
  MetadataSelectors,
} from '@/features/hasura-metadata-api';
import {
  DEFAULT_STALE_TIME,
  useMetadata,
} from '@/features/hasura-metadata-api/useMetadata';
import { Table } from '@/features/hasura-metadata-types';
import { useHttpClient } from '@/features/Network';
import { useEffect } from 'react';
import { useQuery } from 'react-query';

type FetchSuggestedRelationshipsArgs = NetworkArgs & {
  dataSourceName: string;
  driverPrefix?: string;
};

const emptyResponse: SuggestedRelationshipsResponse = { relationships: [] };

const fetchSuggestedRelationships = async ({
  httpClient,
  dataSourceName,
  driverPrefix,
}: FetchSuggestedRelationshipsArgs) => {
  if (!driverPrefix) {
    return Promise.resolve(emptyResponse);
  }
  return (
    await httpClient.post<any, { data: SuggestedRelationshipsResponse }>(
      '/v1/metadata',
      {
        type: `${driverPrefix}_suggest_relationships`,
        version: 1,
        args: {
          omit_tracked: true,
          source: dataSourceName,
        },
      }
    )
  ).data;
};

type UseSuggestedRelationshipsArgs = {
  dataSourceName: string;
  table: Table;
  existingRelationships: LocalRelationship[];
  isEnabled: boolean;
};

type SuggestedRelationshipsResponse = {
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

export const addConstraintName = (
  relationships: SuggestedRelationship[]
): SuggestedRelationshipWithName[] =>
  relationships.map(relationship => {
    const fromColumns = relationship.from.columns.join('_');
    const toTableName = formatRelationToTableName({
      table: relationship.to.table,
      relationshipType: relationship.type,
    });
    const toColumns = relationship.to.columns.join('_');
    const toTableWithColumns = `${toTableName}_${toColumns}`;
    const constraintName = `${fromColumns}_${toTableWithColumns}`;
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

  const dataSourcePrefix = metadataSource?.kind
    ? getDriverPrefix(metadataSource?.kind)
    : undefined;

  const httpClient = useHttpClient();
  const { data, refetch, isLoading } = useQuery<SuggestedRelationshipsResponse>(
    {
      queryKey: ['suggested_relationships', dataSourceName],
      queryFn: async () => {
        if (!isEnabled) {
          return Promise.resolve(emptyResponse);
        }

        const result = await fetchSuggestedRelationships({
          httpClient,
          dataSourceName,
          driverPrefix: dataSourcePrefix,
        });
        return result;
      },
      refetchOnWindowFocus: false,
      staleTime: DEFAULT_STALE_TIME,
    }
  );

  useEffect(() => {
    if (dataSourcePrefix) {
      refetch();
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
    notExistingRelationships
  );

  return {
    suggestedRelationships: relationshipsWithConstraintName,
    isLoadingSuggestedRelationships: isLoading,
  };
};
