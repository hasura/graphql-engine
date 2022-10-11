import { MetadataSelector, useMetadata } from '@/features/MetadataAPI';
import { QualifiedTable } from '@/metadata/types';
import { DataTarget } from '@/features/Datasources';
import { Schema } from '../schema';

interface UseDefaultValuesProps {
  sourceTableInfo: DataTarget;
  existingRelationshipName?: string;
}

const getSchemaKey = (sourceTableInfo: DataTarget) => {
  return 'dataset' in sourceTableInfo ? 'dataset' : 'schema';
};

type RelationshipType =
  | 'create_object_relationship'
  | 'create_array_relationship';

export const useDefaultValues = ({
  sourceTableInfo,
  existingRelationshipName,
}: UseDefaultValuesProps) => {
  const {
    data: metadataTable,
    isLoading,
    isError,
  } = useMetadata(
    MetadataSelector.getTable(sourceTableInfo.database, {
      name: sourceTableInfo.table,
      schema:
        (sourceTableInfo as any).schema ?? (sourceTableInfo as any).dataset,
    })
  );

  const manual_relationships = [
    ...(metadataTable?.object_relationships?.map(rel => ({
      ...rel,
      type: 'create_object_relationship' as RelationshipType,
    })) ?? []),
    ...(metadataTable?.array_relationships?.map(rel => ({
      ...rel,
      type: 'create_array_relationship' as RelationshipType,
    })) ?? []),
  ];

  const relationship = manual_relationships?.find(
    rel => rel.name === existingRelationshipName
  );

  const defaultValues: Schema = {
    relationshipType: relationship?.type ?? 'create_object_relationship',
    relationshipName: existingRelationshipName ?? '',
    source: {
      database: sourceTableInfo.database,
      [getSchemaKey(sourceTableInfo)]:
        (sourceTableInfo as any).dataset ?? (sourceTableInfo as any).schema,
      table: sourceTableInfo.table,
    },
    destination: {
      database: sourceTableInfo.database,
      [getSchemaKey(sourceTableInfo)]:
        (
          relationship?.using?.manual_configuration
            ?.remote_table as QualifiedTable
        )?.schema ?? '',
      table:
        (
          relationship?.using?.manual_configuration
            ?.remote_table as QualifiedTable
        )?.name ?? '',
    },
    mapping: relationship?.using.manual_configuration?.column_mapping ?? {},
  };

  return { data: defaultValues, isLoading, isError };
};
