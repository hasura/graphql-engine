import { forwardRef } from 'react';
import { Role } from './types';
import { usePermissionsFormContext } from '../hooks/usePermissionForm';

interface PermissionsRowNameProps {
  roleName: Role['name'];
  isNew: Role['isNew'];
}

export const PermissionsRowName = forwardRef<
  HTMLInputElement,
  PermissionsRowNameProps
>(({ roleName, isNew }, inputRef) => {
  const { setNewRoleName } = usePermissionsFormContext();
  if (isNew) {
    return (
      <td className="w-0 bg-gray-50 p-sm font-semibold text-muted">
        <input
          ref={inputRef}
          className="block w-64 h-input px-md shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          value={roleName}
          aria-label="create-new-role"
          data-testid="new-role-input"
          placeholder="Create new role..."
          onChange={e => {
            setNewRoleName(e.target.value);
          }}
        />
      </td>
    );
  }

  return (
    <td className="w-0 bg-gray-50 p-md font-semibold text-muted">
      <div className="flex items-center">
        <label className="flex items-center ml-sm">{roleName}</label>
      </div>
    </td>
  );
});
