import {
  DataSource,
  isManualArrayRelationship,
  isManualObjectRelationship,
} from '@/features/DataSource';
import { Table } from '@/features/hasura-metadata-types';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { useMetadata, MetadataSelectors } from '@/features/hasura-metadata-api';
import { LocalRelationship } from '../types';
import {
  adaptLocalArrayRelationshipWithFkConstraint,
  adaptLocalArrayRelationshipWithManualConfiguration,
  adaptLocalObjectRelationshipWithFkConstraint,
  adaptLocalObjectRelationshipWithManualConfigruation,
} from '../utils/adaptResponse';
import {
  DEFAULT_STALE_TIME,
  generateQueryKeys,
} from '../utils/queryClientUtils';

const useFkConstraints = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: generateQueryKeys.fkConstraints({ table, dataSourceName }),
    queryFn: async () => {
      const result = await DataSource(httpClient).getTableFkRelationships({
        dataSourceName,
        table,
      });
      return result;
    },
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
  });
};

export const useListAllDatabaseRelationships = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const {
    data: metadataTable,
    isFetching: isMetadataPending,
    isLoading: isMetadataLoading,
    error: metadataError,
  } = useMetadata(MetadataSelectors.findTable(dataSourceName, table));

  const {
    data: fkConstraints,
    isFetching: isDALIntrospectionPending,
    isLoading: isDALIntrospectionLoading,
    error: dalError,
  } = useFkConstraints({ dataSourceName, table });

  // adapt local array relationships
  const localArrayRelationships = (
    metadataTable?.array_relationships ?? []
  ).map<LocalRelationship>(relationship => {
    if (isManualArrayRelationship(relationship))
      return adaptLocalArrayRelationshipWithManualConfiguration({
        table,
        dataSourceName,
        relationship,
      });

    return adaptLocalArrayRelationshipWithFkConstraint({
      table,
      dataSourceName,
      relationship,
      fkConstraints: fkConstraints ?? [],
    });
  });

  // adapt local object relationships
  const localObjectRelationships = (
    metadataTable?.object_relationships ?? []
  ).map<LocalRelationship>(relationship => {
    if (isManualObjectRelationship(relationship))
      return adaptLocalObjectRelationshipWithManualConfigruation({
        table,
        dataSourceName,
        relationship,
      });
    return adaptLocalObjectRelationshipWithFkConstraint({
      table,
      dataSourceName,
      relationship,
      fkConstraints: fkConstraints ?? [],
    });
  });

  // TODO (post beta release): adapt remote DB relationships

  // TODO (post beta release): adapt remote schema relationships

  return {
    data: [...localArrayRelationships, ...localObjectRelationships],
    isFetching: isMetadataPending || isDALIntrospectionPending,
    isLoading: isMetadataLoading || isDALIntrospectionLoading,
    error: [metadataError, dalError],
  };
};
