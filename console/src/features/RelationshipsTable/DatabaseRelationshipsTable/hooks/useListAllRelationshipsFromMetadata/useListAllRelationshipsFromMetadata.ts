import {
  exportMetadata,
  Table,
  isLocalTableObjectRelationship,
  isManualArrayRelationship,
  isManualObjectRelationship,
  isRemoteDBRelationship,
  isRemoteSchemaRelationship,
  DataSource,
} from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { areTablesEqual } from '../../../utils';
import { Relationship } from '../../types';
import {
  adaptLegacyRemoteSchemaRelationship,
  adaptLocalTableRelationship,
  adaptManualRelationship,
  adaptRemoteDBRelationship,
  adaptRemoteSchemaRelationship,
  adaptSameTableObjectRelationship,
} from './utils';

export const useListAllRelationshipsFromMetadata = (
  dataSourceName: string,
  table: Table
) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: [dataSourceName, 'list_all_relationships'],
    refetchOnWindowFocus: false,
    queryFn: async () => {
      const metadata = await exportMetadata({ httpClient });
      const metadataSource = metadata.sources.find(
        source => source.name === dataSourceName
      );

      const fkRelationships = await DataSource(
        httpClient
      ).getTableFkRelationships({
        dataSourceName,
        table,
      });

      /**
       * If I can't find the source in the metadata, then something is inconsistent. Panic and throw error
       */
      if (!metadataSource)
        throw Error(
          'useListAllRelationshipsFromMetadata: unable to find a metadata.source that matches dataSourceName'
        );

      // FIX ME: use Matt's util function for the equal-to check later
      const metadataTable = metadataSource.tables.find(t => {
        return areTablesEqual(t.table, table);
      });

      /**
       * If I can't find the table in the metadataSource, then something is inconsistent. Panic and throw error
       */
      if (!metadataTable)
        throw Error(
          'useListAllRelationshipsFromMetadata: unable to find a metadata.source.table that matches table'
        );

      /**
       * Collect all relationships - both local & remote
       */
      let relationships: Relationship[] = [];

      /**
       * Remote relationships
       */
      if (metadataTable.remote_relationships) {
        relationships = [
          ...relationships,
          ...metadataTable.remote_relationships.map<Relationship>(
            relationship => {
              if (isRemoteDBRelationship(relationship))
                return adaptRemoteDBRelationship(
                  dataSourceName,
                  table,
                  relationship
                );

              if (isRemoteSchemaRelationship(relationship))
                return adaptRemoteSchemaRelationship(
                  dataSourceName,
                  table,
                  relationship
                );

              // If its neither of those two cases, then it must be a legacy DB-to-RS relationship
              return adaptLegacyRemoteSchemaRelationship(
                dataSourceName,
                table,
                relationship
              );
            }
          ),
        ];
      }

      /**
       * Local Object relationships
       */
      if (metadataTable.object_relationships) {
        relationships = [
          ...relationships,
          ...metadataTable.object_relationships.map<Relationship>(
            relationship => {
              if (isManualObjectRelationship(relationship))
                return adaptManualRelationship(
                  dataSourceName,
                  table,
                  relationship
                );

              /**
               * To a local table via FK relationships
               */
              if (isLocalTableObjectRelationship(relationship))
                return adaptLocalTableRelationship(
                  dataSourceName,
                  table,
                  relationship,
                  fkRelationships
                );

              /**
               * If its not both the above cases then its a object relationships between columns in the same table
               */
              return adaptSameTableObjectRelationship(
                dataSourceName,
                table,
                relationship,
                fkRelationships
              );
            }
          ),
        ];
      }

      /**
       * Local Array relationships
       */
      if (metadataTable.array_relationships) {
        relationships = [
          ...relationships,
          ...metadataTable.array_relationships.map<Relationship>(
            relationship => {
              if (isManualArrayRelationship(relationship))
                return adaptManualRelationship(
                  dataSourceName,
                  table,
                  relationship
                );

              return adaptLocalTableRelationship(
                dataSourceName,
                table,
                relationship,
                fkRelationships
              );
            }
          ),
        ];
      }
      return relationships;
    },
  });
};
