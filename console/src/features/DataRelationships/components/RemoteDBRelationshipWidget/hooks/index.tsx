import { MetadataSelector, useMetadata } from '@/features/MetadataAPI';
import { QualifiedTable } from '@/metadata/types';
import { DataTarget } from '@/features/Datasources';
import { Schema } from '../schema';
import { getSchemaKey } from '../utils';

interface UseDefaultValuesProps {
  sourceTableInfo: DataTarget;
  existingRelationshipName?: string;
}

type MetadataTableType = {
  dataset?: string;
  schema?: string;
  name: string;
};
type RelationshipType = 'object' | 'array';

export const useDefaultValues = ({
  sourceTableInfo,
  existingRelationshipName,
}: UseDefaultValuesProps) => {
  const { data: metadataTable, isLoading, isError } = useMetadata(
    MetadataSelector.getTable(sourceTableInfo.database, {
      name: sourceTableInfo.table,
      [getSchemaKey(sourceTableInfo)]:
        (sourceTableInfo as any).schema ?? (sourceTableInfo as any).dataset,
    } as QualifiedTable)
  );

  const remote_db_relationships = metadataTable?.remote_relationships?.filter(
    rel => 'to_source' in rel.definition
  );

  const relationship = remote_db_relationships?.find(
    rel => rel.name === existingRelationshipName
  );

  const defaultValues: Schema = {
    relationshipType:
      (relationship?.definition?.to_source
        ?.relationship_type as RelationshipType) ?? 'object',
    relationshipName: existingRelationshipName ?? '',
    source: {
      database: sourceTableInfo.database,
      [getSchemaKey(sourceTableInfo)]:
        (sourceTableInfo as any).dataset ?? (sourceTableInfo as any).schema,
      table: sourceTableInfo.table,
    },
    destination: {
      database: relationship?.definition?.to_source?.source ?? '',
      [relationship?.definition?.to_source.source === 'bigquery'
        ? 'dataset'
        : 'schema']:
        (relationship?.definition?.to_source?.table as MetadataTableType)
          ?.dataset ??
        (relationship?.definition?.to_source?.table as MetadataTableType)
          ?.schema ??
        '',
      table: relationship?.definition?.to_source?.table.name ?? '',
    },
    mapping: relationship?.definition?.to_source?.field_mapping ?? {},
  };

  return { data: defaultValues, isLoading, isError };
};
