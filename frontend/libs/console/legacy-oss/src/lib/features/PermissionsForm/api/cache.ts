import { useQueryClient } from 'react-query';
import produce from 'immer';

import {
  DeletePermissionEntry,
  InsertPermissionEntry,
  SelectPermissionEntry,
  UpdatePermissionEntry,
} from '@/metadata/types';

import { MetadataResponse } from '../../MetadataAPI';
import { api } from './api';
import { AccessType, QueryType } from '../types';

type PermissionEntry =
  | InsertPermissionEntry
  | SelectPermissionEntry
  | UpdatePermissionEntry
  | DeletePermissionEntry;

type MetadataKeys =
  | 'insert_permissions'
  | 'select_permissions'
  | 'update_permissions'
  | 'delete_permissions';

interface UpdateTablePermissionArgs {
  key: MetadataKeys;
  tableName: string;
  roleName: string;
  metadata: MetadataResponse;
  data: ReturnType<typeof api.createInsertBody>;
}

export const updateTablePermission = ({
  key,
  tableName,
  roleName,
  metadata,
  data,
}: UpdateTablePermissionArgs) => {
  // find the arg
  const newMetadataItem = data.args.find(arg => arg.type.includes('create'));

  // find and update the relevant piece of metadata that needs updating
  const nextState = produce(metadata, draft => {
    // find the table that is being edited
    const selectedTable = draft.metadata.sources[0].tables.find(
      ({ table }) => table.name === tableName
    );

    // find the queryType that is being edited
    const selectedPermission = selectedTable?.[key];

    // find the index of the role that is being edited
    const selectedRolePermissionIndex = selectedPermission?.findIndex(
      (permission: PermissionEntry) => permission.role === roleName
    );

    // if the selected permission already exists replace it
    if (
      selectedRolePermissionIndex !== undefined &&
      selectedPermission &&
      newMetadataItem
    ) {
      selectedPermission[selectedRolePermissionIndex] = newMetadataItem?.args;
    } else if (newMetadataItem) {
      selectedPermission?.push(newMetadataItem.args);
    }
  });

  return nextState;
};

interface HandleUpdateArgs {
  args: {
    tableName: string;
    schemaName: string;
    roleName: string;
    queryType: QueryType;
    accessType: AccessType;
  };
  response: {
    headers: any;
    body: ReturnType<typeof api.createInsertBody>;
  };
}

const useUpdateTablePermissionCache = () => {
  const client = useQueryClient();

  const handleUpdate = ({ args, response }: HandleUpdateArgs) => {
    const metadata = client.getQueryData<MetadataResponse>(['metadata']);

    const { tableName, roleName, queryType } = args;

    if (metadata) {
      // update cache
      const result = updateTablePermission({
        key: `${queryType}_permissions`,
        tableName,
        roleName,
        metadata,
        data: response.body,
      });

      client.setQueryData('metadata', result);
    }

    return { metadata };
  };

  return { handleUpdate };
};

export const cache = {
  useUpdateTablePermissionCache,
};
