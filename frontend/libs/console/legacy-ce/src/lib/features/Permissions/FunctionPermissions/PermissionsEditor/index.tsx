import React, { useState } from 'react';
import PermissionEditor from './PermissionEditor';
import { useAppContext } from '@hasura/shared/context';
import {
  useCreateFunctionPermission,
  useDropFunctionPermission,
} from '@hasura/metadata/api';
import PermissionsTable from './PermissionsTable';
import { getRoleQueryPermissionSymbol } from './utils';
import { useDataSourceContext } from '../../../Data/context/DataSourceContext';
import { MetadataFunction } from '@hasura/shared/types';
import { areTablesEqual, MetadataSelectors } from '@hasura/metadata/helpers';

interface PermissionsProps {
  currentFunction: MetadataFunction;
  isPermissionsEditable: boolean;
  tableName?: string;
}

const Permissions: React.FC<PermissionsProps> = ({
  currentFunction,
  isPermissionsEditable,
  tableName,
}) => {
  const { readOnlyMode } = useAppContext();
  const { currentSource, metadata } = useDataSourceContext();
  const { createFunctionPermission, isPending: isCreating } =
    useCreateFunctionPermission();
  const { dropFunctionPermission, isPending: isDropping } =
    useDropFunctionPermission();

  const [isEditing, setIsEditing] = useState(false);
  const [role, setRole] = useState('');

  const allRoles = MetadataSelectors.getRoles(metadata);
  const tableSelectPermissions = currentFunction.configuration?.response?.table
    ? currentSource.tables.find((t) =>
        areTablesEqual(t.table, currentFunction.configuration!.response!.table),
      )?.select_permissions
    : [];

  const permCloseEdit = () => {
    setIsEditing(false);
    setRole('');
  };

  const permOpenEdit = (r: string) => {
    setIsEditing(true);
    setRole(r);
  };

  const allPermissions = currentFunction.permissions;
  const permissionAccessString = getRoleQueryPermissionSymbol(
    allPermissions,
    role,
    tableSelectPermissions,
    isPermissionsEditable,
  );

  const saveFunc = () =>
    createFunctionPermission(
      {
        dataSourceName: currentSource.name,
        qualifiedFunction: currentFunction.function,
        role,
      },
      { onSuccess: permCloseEdit },
    );

  const removeFunc = () =>
    dropFunctionPermission(
      {
        dataSourceName: currentSource.name,
        qualifiedFunction: currentFunction.function,
        role,
      },
      {
        onSuccess: permCloseEdit,
      },
    );

  return (
    <>
      <PermissionsTable
        permCloseEdit={permCloseEdit}
        permOpenEdit={permOpenEdit}
        isEditing={isEditing}
        role={role}
        allRoles={allRoles}
        readOnlyMode={readOnlyMode}
        allPermissions={allPermissions}
        isEditable={isPermissionsEditable}
        selectRoles={tableSelectPermissions}
      />
      <div className="mb-6">
        {!readOnlyMode && (
          <PermissionEditor
            saveFn={saveFunc}
            removeFn={removeFunc}
            closeFn={permCloseEdit}
            role={role}
            isEditing={isEditing}
            permissionAccessInMetadata={permissionAccessString}
            table={tableName ?? ''}
            isSaving={isCreating}
            isDeleting={isDropping}
          />
        )}
      </div>
    </>
  );
};

export default Permissions;
