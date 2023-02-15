import { MetadataTable } from '@/features/hasura-metadata-types';

export const getMetadataTableCustomName = (
  metadataTable: MetadataTable | undefined
) => {
  return metadataTable?.configuration?.custom_name;
};
