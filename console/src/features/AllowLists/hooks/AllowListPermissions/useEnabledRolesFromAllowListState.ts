import React, { useEffect } from 'react';
import { useRoles } from '@/features/MetadataAPI';
import { useEnabledRolesFromAllowList } from './useEnabledRolesFromAllowList';

export const useEnabledRolesFromAllowListState = (collectionName: string) => {
  const { data: allAvailableRoles } = useRoles();
  const { data: allowListRoles } = useEnabledRolesFromAllowList(collectionName);
  const [enabledRoles, setEnabledRoles] = React.useState<string[]>([]);
  const [newRoles, setNewRoles] = React.useState<string[]>(['']);

  useEffect(() => {
    if (allowListRoles && allowListRoles !== enabledRoles) {
      setEnabledRoles(allowListRoles);
    }
  }, [allowListRoles]);

  return {
    allAvailableRoles,
    newRoles,
    setNewRoles,
    enabledRoles,
    setEnabledRoles,
  };
};
