import { ReactNode } from 'react';
import { Button } from '../../../../new-components/Button';
import { IconTooltip } from '../../../../new-components/Tooltip';
import { usePermissionsFormContext } from '../hooks/usePermissionForm';
import { Permission } from './types';
import { Collapse } from '../../../../new-components/deprecated';
import { getEdForm } from '../../../../components/Services/Data/utils';
import { Badge } from '../../../../new-components/Badge';

type PermissionsFormProps = {
  permission: Permission;
  onSave: () => void;
  onDelete: () => void;
  PermissionsInput: ReactNode;
  isCreating?: boolean;
  isRemoving?: boolean;
};

export const PermissionsForm = ({
  permission,
  onDelete,
  onSave,
  PermissionsInput,
  isCreating,
  isRemoving,
}: PermissionsFormProps) => {
  const {
    unsetActivePermission,
    rowSelectPermissions,
    setRowSelectPermissions,
    columns,
    toggleColumn,
    toggleAllColumns,
    columnPermissionsStatus,
    setPermission,
  } = usePermissionsFormContext();
  return (
    <form
      onSubmit={e => {
        e.preventDefault();
        onSave();
      }}
    >
      <div
        className="bg-white rounded p-md border border-gray-300"
        data-testid="permissions-form"
      >
        <div className="pb-4 flex items-center gap-4">
          <Button
            type="button"
            onClick={() => {
              unsetActivePermission();
            }}
          >
            Close
          </Button>
          <h3 data-testid="form-title">
            <strong>Role:</strong>
            <Badge className="mx-2" data-testid="role-pill">
              {permission.roleName}
            </Badge>
            <strong>Action:</strong>
            <Badge className="mx-2" data-testid="action-pill">
              {permission.action}
            </Badge>
          </h3>
        </div>
        <fieldset className="grid gap-2">
          <div>
            <label className="flex items-center gap-2">
              <input
                id={'without_filter'}
                type="radio"
                value={'without_filter'}
                checked={rowSelectPermissions === 'without_filter'}
                data-testid="without-filter"
                onClick={() => {
                  setRowSelectPermissions('without_filter');
                  setPermission(permission.roleName, {});
                }}
              />
              <NoChecksLabel />
            </label>
          </div>

          <div>
            <label className="flex items-center gap-2">
              <input
                id={'with_custom_filter'}
                type="radio"
                value={rowSelectPermissions}
                checked={rowSelectPermissions === 'with_custom_filter'}
                onClick={() => {
                  setRowSelectPermissions('with_custom_filter');
                }}
              />
              <CustomLabel />
            </label>

            {rowSelectPermissions === 'with_custom_filter' && (
              <div className="pt-4">
                <div>{PermissionsInput}</div>
              </div>
            )}
          </div>
        </fieldset>

        <Collapse defaultOpen>
          <Collapse.Header
            title={`Column ${permission.action} permissions`}
            tooltip={`Choose columns allowed to be ${getEdForm(
              permission.action
            )}`}
            status={columnPermissionsStatus(permission)}
            disabledMessage="Set row permissions first"
          />
          <Collapse.Content>
            <div className="grid gap-2">
              <div className="flex gap-2 items-center">
                <p>
                  Allow role <strong>{permission.roleName}</strong> to access{' '}
                  <strong>columns</strong>:
                </p>
              </div>
              <fieldset className="flex gap-4 flex-wrap">
                {columns?.map(column => (
                  <label key={column} className="flex gap-2 items-center">
                    <input
                      type="checkbox"
                      data-testid={`column-${column}-checkbox`}
                      style={{ marginTop: '0px !important' }}
                      className="rounded shadow-sm border border-gray-300 hover:border-gray-400 focus:ring-yellow-400"
                      checked={permission.columns.includes(column)}
                      onChange={() => {
                        toggleColumn(permission, column);
                      }}
                    />
                    <i>{column}</i>
                  </label>
                ))}
                <Button
                  type="button"
                  size="sm"
                  onClick={() => toggleAllColumns(permission)}
                  data-testid="toggle-all-columns"
                >
                  Toggle All
                </Button>
              </fieldset>
            </div>
          </Collapse.Content>
        </Collapse>

        <div className="pt-2 flex gap-2 mt-4" id="form-buttons-container">
          <Button
            isLoading={isCreating}
            disabled={isRemoving || isCreating}
            type="submit"
            mode="primary"
            title={'Submit'}
            data-testid="save-permissions-button"
          >
            Save Permissions
          </Button>

          <Button
            isLoading={isRemoving}
            disabled={isRemoving || isCreating}
            type="button"
            mode="destructive"
            onClick={onDelete}
            data-testid="delete-permissions-button"
          >
            Delete Permissions
          </Button>
        </div>
      </div>
    </form>
  );
};

const NoChecksLabel = () => (
  <span data-test="without-checks">Without any checks&nbsp;</span>
);

const CustomLabel = () => (
  <span data-test="custom-check" className="flex items-center">
    With custom check:
    <IconTooltip message="Create custom check using permissions builder" />
  </span>
);
