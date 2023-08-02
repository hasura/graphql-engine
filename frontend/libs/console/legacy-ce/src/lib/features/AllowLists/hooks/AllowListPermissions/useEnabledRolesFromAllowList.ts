import { MetadataSelector, useMetadata } from '../../../MetadataAPI';

export const useEnabledRolesFromAllowList = (queryCollectionName: string) =>
  useMetadata(MetadataSelector.getNewRolePermission(queryCollectionName));
