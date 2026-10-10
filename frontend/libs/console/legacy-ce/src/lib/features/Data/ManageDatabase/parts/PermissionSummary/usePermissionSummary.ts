import {
  useBulkCopyPermissionsByRole,
  useBulkDeletePermissions,
} from '@hasura/metadata/api';
import { useMemo, useState } from 'react';
import {
  keyToPermission,
  MetadataPermissionKey,
  metadataPermissionKeys,
  Source,
} from '@hasura/shared/types';
import { useDestructiveConfirm, SelectItemProps } from '@hasura/shared/ui';
import { useDriverCapabilities } from '@hasura/metadata/data-source';

type Props = {
  source: Source;
};

const usePermissionSummary = ({ source }: Props) => {
  const { data: capabilities, isFetching: capabilitiesFetching } =
    useDriverCapabilities({ source });
  const [selectedPermissionType, setSelectedPermissionType] =
    useState<MetadataPermissionKey>('select_permissions');

  const destructiveConfirm = useDestructiveConfirm();

  const { mutateAsync: bulkDeletePermissions, isPending: isDeleting } =
    useBulkDeletePermissions({
      dataSourceName: source.name,
      table: null,
    });

  const { isPending: isCopying } = useBulkCopyPermissionsByRole();

  const permissionTypes = useMemo(() => {
    const result: SelectItemProps[] = [];
    if (capabilities?.queries) {
      result.push({
        label: 'Select permissions',
        value: 'select_permissions',
      });
    }

    if (capabilities?.mutations?.insert) {
      result.push({
        label: 'Insert permissions',
        value: 'insert_permissions',
      });
    }

    if (capabilities?.mutations?.update) {
      result.push({
        label: 'Update permissions',
        value: 'update_permissions',
      });
    }

    if (capabilities?.mutations?.delete) {
      result.push({
        label: 'Delete permissions',
        value: 'delete_permissions',
      });
    }

    return result;
  }, [capabilities]);

  const roles = useMemo((): string[] => {
    const roles = new Set<string>();

    source.tables.forEach((table) => {
      metadataPermissionKeys.forEach((permissionType) => {
        const permissions = table[permissionType];
        permissions?.forEach((permission) => {
          roles.add(permission.role);
        });
      });
    });

    return Array.from(roles);
  }, [source]);

  const handleDelete = (role: string) => {
    destructiveConfirm({
      resourceName: role,
      resourceType: `all ${keyToPermission[selectedPermissionType]} permissions of role`,
      destroyTerm: 'delete',
      onConfirm: () => {
        return bulkDeletePermissions({
          roles: [role],
          permissionKeys: [selectedPermissionType],
        })
          .then(() => true)
          .catch(() => false);
      },
    });
  };

  return {
    roles,
    permissionTypes,
    selectedPermissionType,
    setSelectedPermissionType,
    isDeleting,
    isCopying,
    handleDelete,
    isFetching: capabilitiesFetching,
  };
};

export default usePermissionSummary;
