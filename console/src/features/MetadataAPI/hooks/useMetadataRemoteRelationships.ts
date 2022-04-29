import { MetadataSelector } from './metadataSelectors';
import { MetadataTransformer } from './metadataTransformers';
import { useMetadata } from './useMetadata';

interface UseDbToRemoteSchemaRelationshipsArgs {
  dataSource: string;
  tableName: string;
}

export const useDbToRemoteSchemaRelationships = ({
  dataSource,
  tableName,
}: UseDbToRemoteSchemaRelationshipsArgs) => {
  const { data, ...rest } = useMetadata(
    MetadataSelector.getDbToRemoteSchemaRelationships({
      dataSource,
      tableName,
    })
  );

  // the transform function could also be passed into use metadata as a second argument
  // if we choose to use this method
  if (data?.table && data?.remote_relationships) {
    const { table, remote_relationships } = data;
    const transformedData = MetadataTransformer.transformDbToRemoteSchema({
      table,
      remote_relationships,
    });

    return { data: transformedData, ...rest };
  }

  return { data: [], ...rest };
};
