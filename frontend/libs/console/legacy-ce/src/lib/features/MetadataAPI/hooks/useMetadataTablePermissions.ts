import { QualifiedTable } from '../../../metadata/types';
import { MetadataSelector } from './metadataSelectors';
import { MetadataTransformer } from './metadataTransformers';
import { useMetadata } from './useMetadata';

export const useMetadataTablePermissions = (
  table: QualifiedTable,
  dataSource: string
) => {
  return useMetadata(MetadataSelector.getTablePermissions(dataSource, table));
};

export const useMetadataPermissions = (dataSource: string) => {
  return useMetadata(
    MetadataSelector.getTables(dataSource),
    MetadataTransformer.transformPermissions
  );
};
