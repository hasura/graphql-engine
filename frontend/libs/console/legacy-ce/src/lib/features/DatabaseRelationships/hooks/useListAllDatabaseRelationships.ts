import { Table } from '../../hasura-metadata-types';
import {
  useMetadata,
  MetadataSelectors,
  areTablesEqual,
} from '../../hasura-metadata-api';
import { useSuggestedRelationships } from '../../Data/TrackResources/TrackRelationships/hooks/useSuggestedRelationships';
import {
  isLegacyRemoteSchemaRelationship,
  isManualArrayRelationship,
  isManualObjectRelationship,
  isRemoteSchemaRelationship,
} from '../../DataSource';
import {
  adaptLegacyRemoteSchemaRelationship,
  adaptLocalArrayRelationshipWithManualConfiguration,
  adaptLocalObjectRelationshipWithManualConfiguration,
  adaptRemoteDatabaseRelationship,
  adaptRemoteSchemaRelationship,
} from '../utils/adaptResponse';
import { LocalRelationship } from '../types';

export const useListAllDatabaseRelationships = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const {
    data: { tracked = [] } = {},
    isLoading: isSuggestedRelationshipsLoading,
    isFetching: isSuggestedRelationshipsFetching,
    error: suggestedRelationshipsError,
  } = useSuggestedRelationships({
    dataSourceName,
    which: 'all',
  });

  const filteredTrackedFkRels: LocalRelationship[] = tracked
    .filter(rel => areTablesEqual(rel.fromTable, table))
    .map(rel => ({
      name: rel.name,
      fromSource: dataSourceName,
      fromTable: rel.fromTable,
      type: 'localRelationship',
      relationshipType: rel.type === 'object' ? 'Object' : 'Array',
      definition: {
        toTable: rel.toTable,
        mapping: rel.columnMapping,
      },
    }));

  const {
    data: relationshipsWithManualConfigs = [],
    isLoading: isMetadataLoading,
    isFetching: isMetadataFetching,
    error: metadataError,
  } = useMetadata(m => {
    const metadataTable = MetadataSelectors.findTable(dataSourceName, table)(m);

    const localArrayRelationshipsWithManualConfig = (
      metadataTable?.array_relationships ?? []
    )
      .filter(isManualArrayRelationship)
      .map(relationship =>
        adaptLocalArrayRelationshipWithManualConfiguration({
          table,
          dataSourceName,
          relationship,
        })
      );

    const localObjectRelationshipsWithManualConfig = (
      metadataTable?.object_relationships ?? []
    )
      .filter(isManualObjectRelationship)
      .map(relationship =>
        adaptLocalObjectRelationshipWithManualConfiguration({
          table,
          dataSourceName,
          relationship,
        })
      );

    const remoteRels = (metadataTable?.remote_relationships ?? []).map(
      relationship => {
        if (isRemoteSchemaRelationship(relationship))
          return adaptRemoteSchemaRelationship({
            table,
            dataSourceName,
            relationship,
          });

        if (isLegacyRemoteSchemaRelationship(relationship))
          return adaptLegacyRemoteSchemaRelationship({
            table,
            dataSourceName,
            relationship,
          });

        return adaptRemoteDatabaseRelationship({
          table,
          dataSourceName,
          relationship,
        });
      }
    );

    return [
      ...remoteRels,
      ...localArrayRelationshipsWithManualConfig,
      ...localObjectRelationshipsWithManualConfig,
    ];
  });

  return {
    data: [...filteredTrackedFkRels, ...relationshipsWithManualConfigs],
    isLoading: isSuggestedRelationshipsLoading || isMetadataLoading,
    isFetching: isSuggestedRelationshipsFetching || isMetadataFetching,
    error: [metadataError, suggestedRelationshipsError],
  };
};
