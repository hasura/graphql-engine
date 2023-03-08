import { useSubmitForm } from './useSubmitForm';
import { useDeletePermission } from './useDeletePermission';

import { AccessType, QueryType } from '../../../types';
import { Table } from '../../../../hasura-metadata-types';

export interface UseUpdatePermissionsArgs {
  dataSourceName: string;
  table: Table;
  tables: Table[];
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

export const useUpdatePermissions = ({
  dataSourceName,
  table,
  tables,
  roleName,
  queryType,
  accessType,
}: UseUpdatePermissionsArgs) => {
  const updatePermissions = useSubmitForm({
    dataSourceName,
    table,
    tables,
    roleName,
    queryType,
    accessType,
  });

  const deletePermissions = useDeletePermission({
    dataSourceName,
    table,
    roleName,
  });

  return {
    updatePermissions,
    deletePermissions,
  };
};
