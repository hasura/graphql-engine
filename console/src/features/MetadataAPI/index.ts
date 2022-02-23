export { MetadataSelector } from './hooks/metadataSelectors';
export { useRoles } from './hooks/useRoles';
export {
  useMetadataTables,
  useRemoteDatabaseRelationships,
  useRemoteSchemaRelationships,
} from './hooks/useMetadataTables';
export { useMetadataVersion } from './hooks/useMetadataVersion';
export { useMetadataTableComputedFields } from './hooks/useMetadataTableComputedFields';
export { useMetadataTablePermissions } from './hooks/useMetadataTablePermissions';
export { useMetadataSource } from './hooks/useMetadataSource';

// we probably need to remove all usages that directly use the useMetadata and use only the exported functions
export { useMetadata } from './hooks/useMetadata';
export { useMetadataMigration } from './hooks/useMetadataMigration';

export * from './types';
