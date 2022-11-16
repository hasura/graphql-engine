import {
  DataSource,
  isManualArrayRelationship,
  isManualObjectRelationship,
} from '@/features/DataSource';
import { Table } from '@/features/hasura-metadata-types';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { useMetadataTable } from '@/features/hasura-metadata-api';
import { LocalRelationship, Relationship } from '../types';
import {
  adaptLocalArrayRelationshipWithFkConstraint,
  adaptLocalArrayRelationshipWithManualConfiguration,
  adaptLocalObjectRelationshipWithFkConstraint,
  adaptLocalObjectRelationshipWithManualConfigruation,
} from '../utils/adaptResponse';
import { generateQueryKeys } from '../utils/queryClientUtils';

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
  });
};

export const useListAllDatabaseRelationships = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const { data: metadataTable, isFetching: isMetadataPending } =
    useMetadataTable(dataSourceName, table);

  const { data: fkConstraints, isFetching: isDALIntrospectionPending } =
    useFkConstraints({ dataSourceName, table });

  return useQuery<Relationship[], Error>({
    queryKey: generateQueryKeys.allRelationships({ dataSourceName, table }),
    queryFn: async () => {
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

      return [...localArrayRelationships, ...localObjectRelationships];
    },
    enabled: !isMetadataPending && !isDALIntrospectionPending,
    refetchOnWindowFocus: false,
  });
};
