import { MetadataSelector, useMetadata } from '@/features/MetadataAPI';

export const useEnabledRolesFromAllowList = (queryCollectionName: string) =>
  useMetadata(MetadataSelector.getNewRolePermission(queryCollectionName));
