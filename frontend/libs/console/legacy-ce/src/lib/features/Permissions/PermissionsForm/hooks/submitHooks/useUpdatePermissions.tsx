import { useSubmitForm } from './useSubmitForm';
import { useDeletePermission } from './useDeletePermission';

import { AccessType, QueryType } from '../../../types';
import { Table } from '../../../../hasura-metadata-types';
import { z } from 'zod';
import { inputValidationSchema } from '../../../../../components/Services/Data/TablePermissions/InputValidation/InputValidation';

export interface UseUpdatePermissionsArgs {
  dataSourceName: string;
  table: Table;
  tables: Table[];
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
  validateInput?: z.infer<typeof inputValidationSchema>;
}

export const useUpdatePermissions = ({
  dataSourceName,
  table,
  tables,
  roleName,
  queryType,
  accessType,
  validateInput,
}: UseUpdatePermissionsArgs) => {
  const updatePermissions = useSubmitForm({
    dataSourceName,
    table,
    tables,
    roleName,
    queryType,
    accessType,
    validateInput,
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
