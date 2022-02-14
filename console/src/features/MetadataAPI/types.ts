import { HasuraMetadataV3 } from '@/metadata/types';

export interface MetadataResponse {
  resource_version: number;
  metadata: HasuraMetadataV3;
}
