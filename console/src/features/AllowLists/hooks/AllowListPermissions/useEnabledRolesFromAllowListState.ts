import React from 'react';
import { useRoles } from '@/features/MetadataAPI';
import { useEnabledRolesFromAllowList } from './useEnabledRolesFromAllowList';

export const useEnabledRolesFromAllowListState = (collectionName: string) => {
  const { data: allAvailableRoles } = useRoles();
  const { data: enabledRoles } = useEnabledRolesFromAllowList(collectionName);
  const [newRoles, setNewRoles] = React.useState<string[]>(['']);

  return {
    allAvailableRoles,
    enabledRoles: enabledRoles || [],
    newRoles: newRoles.filter(role => !allAvailableRoles.includes(role)),
    setNewRoles,
  };
};
