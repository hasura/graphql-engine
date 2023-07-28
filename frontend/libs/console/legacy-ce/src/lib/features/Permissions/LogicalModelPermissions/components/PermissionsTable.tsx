import { createRef } from 'react';
import { Action, Permission } from './types';
import { PermissionsRow } from './PermissionsRow';
import { PermissionsIcon } from '../../PermissionsTable/components/PermissionsIcons';

export type PermissionsTableProps = {
  allowedActions: Action[];
  permissions: Permission[];
};

export const PermissionsTable = ({
  allowedActions,
  permissions,
}: PermissionsTableProps) => {
  const actions = ['insert', 'select', 'update', 'delete'];
  const inputRef = createRef<HTMLInputElement>();
  return (
    <div className="grid gap-2" data-testid="permissions-table">
      <div className="flex gap-4">
        <span>
          <PermissionsIcon type="fullAccess" />
          &nbsp;-&nbsp;full access
        </span>
        <span>
          <PermissionsIcon type="noAccess" />
          &nbsp;-&nbsp;no access
        </span>
        <span>
          <PermissionsIcon type="partialAccess" />
          &nbsp;-&nbsp;partial access
        </span>
      </div>
      <div className="overflow-x-auto border border-gray-300 rounded">
        <table className="min-w-full divide-y divide-gray-200 text-left">
          <thead>
            <tr className="divide-x divide-gray-300">
              <th className="w-0 bg-gray-50 border-r border-gray-200 px-md py-sm text-sm font-semibold text-muted uppercase tracking-wider">
                ROLE
              </th>
              {actions.map(action => (
                <th
                  className="bg-gray-50 px-md py-sm text-sm font-semibold text-muted text-center uppercase tracking-wider"
                  key={action}
                >
                  {action.toUpperCase()}
                </th>
              ))}
            </tr>
          </thead>

          <tbody className="bg-white divide-y divide-gray-300">
            {permissions.map((permission, index) => (
              // Using index as key instead of role.name because role.name is editable
              <PermissionsRow
                key={index}
                index={index}
                ref={inputRef}
                permission={permission}
                actions={actions}
                allowedActions={allowedActions}
              />
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
};
