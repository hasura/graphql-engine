import { DataTarget } from '../../Datasources';
import { MetadataSelector } from './metadataSelectors';
import { MetadataTransformer } from './metadataTransformers';
import { useMetadata } from './useMetadata';

export const useDbToRemoteSchemaRelationships = (target: DataTarget) => {
  const { data, ...rest } = useMetadata(
    MetadataSelector.getDbToRemoteSchemaRelationships(target)
  );

  // the transform function could also be passed into use metadata as a second argument
  // if we choose to use this method
  if (target.table && data) {
    const transformedData = MetadataTransformer.transformDbToRemoteSchema({
      target,
      remote_relationships: data,
    });

    return { data: transformedData, ...rest };
  }

  return { data: [], ...rest };
};

export const useDbToRemoteDbRelationships = (target: DataTarget) => {
  const { data, ...rest } = useMetadata(
    MetadataSelector.getRemoteDatabaseRelationships({ target })
  );

  if (target.table && data) {
    const transformedData = MetadataTransformer.transformDbToDb({
      target,
      remote_relationships: data,
    });
    return { data: transformedData, ...rest };
  }

  return { data: [], ...rest };
};
