import { useCallback } from 'react';
import {
  Comparators,
  RowPermissionsInput,
} from '../PermissionsForm/components/RowPermissionsBuilder/components';
import {
  LogicalModelWithPermissions,
  OnDelete,
  OnSave,
  Permission,
  RowSelectPermissionsType,
} from './components/types';
import {
  PermissionsTable,
  PermissionsTableProps,
} from './components/PermissionsTable';
import { PermissionsForm } from './components/PermissionsForm';
import { usePermissionsFormContext } from './hooks/usePermissionForm';
import { LogicalModelPermissionsFormProvider } from './components/LogicalModelPermissionsFormProvider';

export type LogicalModelPermissionsProps = {
  logicalModelName: string;
  logicalModels: LogicalModelWithPermissions[];
  onSave: OnSave;
  onDelete: OnDelete;
  comparators: Comparators;
  isCreating?: boolean;
  isRemoving?: boolean;
  roles: string[];
};

export type LogicalModelPermissionsState = {
  permissions: Permission[];
  activePermission: number | null;
  rowSelectPermissions: RowSelectPermissionsType;
  columns: string[];
};

export const LogicalModelPermissions = ({
  logicalModelName,
  logicalModels,
  onSave,
  onDelete,
  comparators,
  isCreating,
  isRemoving,
  roles,
}: LogicalModelPermissionsProps) => {
  const logicalModel = logicalModels.find(
    model => model.name === logicalModelName
  );

  return (
    <LogicalModelPermissionsFormProvider
      logicalModel={logicalModel}
      roles={roles}
    >
      <PureLogicalModelPermissions
        onSave={onSave}
        onDelete={onDelete}
        isCreating={isCreating}
        isRemoving={isRemoving}
        comparators={comparators}
        logicalModelName={logicalModelName}
        logicalModels={logicalModels}
        roles={roles}
      />
    </LogicalModelPermissionsFormProvider>
  );
};

const PureLogicalModelPermissions = ({
  onSave: onSaveProp,
  onDelete: onDeleteProp,
  logicalModelName,
  comparators,
  logicalModels,
  isCreating,
  isRemoving,
}: LogicalModelPermissionsProps) => {
  const { activePermission, permissions, setPermission } =
    usePermissionsFormContext();
  const permission =
    activePermission === null ? null : permissions[activePermission];
  const onSave = useCallback(async () => {
    if (!permission) {
      return;
    }
    await onSaveProp(permission);
  }, [onSaveProp, permission]);
  const onDelete = useCallback(async () => {
    if (!permission) {
      return;
    }
    await onDeleteProp(permission);
  }, [onDeleteProp, permission]);
  const allowedActions: PermissionsTableProps['allowedActions'] = ['select'];
  return (
    <div className="p-4">
      <div className="grid gap-4">
        <PermissionsTable
          allowedActions={allowedActions}
          permissions={permissions}
        />
        {permission && (
          <PermissionsForm
            permission={permission}
            onSave={onSave}
            onDelete={onDelete}
            isCreating={isCreating}
            isRemoving={isRemoving}
            PermissionsInput={
              <RowPermissionsInput
                table={undefined}
                forbidden={['exists']}
                tables={[]}
                onPermissionsChange={permissionFilter => {
                  setPermission(permission.roleName, permissionFilter);
                }}
                logicalModel={logicalModelName}
                logicalModels={logicalModels}
                permissions={permission.filter}
                comparators={comparators}
              />
            }
          />
        )}
      </div>
    </div>
  );
};
