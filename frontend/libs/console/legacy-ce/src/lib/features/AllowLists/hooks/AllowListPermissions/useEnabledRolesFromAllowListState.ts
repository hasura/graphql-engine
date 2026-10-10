import React from 'react';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';

export const useEnabledRolesFromAllowListState = (collectionName: string) => {
  const { data, ...rest } = useMetadata();
  const [newRoles, setNewRoles] = React.useState<string[]>(['']);
  const allAvailableRoles = data ? MetadataSelectors.selectRoles(data) : [];
  const enabledRoles = data
    ? MetadataSelectors.getNewRolePermission(collectionName)(data)
    : [];

  return {
    allAvailableRoles,
    enabledRoles: enabledRoles || [],
    newRoles: newRoles.filter((role) => !allAvailableRoles.includes(role)),
    setNewRoles,
    ...rest,
  };
};
