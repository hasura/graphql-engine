import React from 'react';
import { Switch } from '@/new-components/Switch';
import { FaCheck } from 'react-icons/fa';
import { useSetRoleToAllowListPermission } from '../../hooks/AllowListPermissions/useSetRoleToAllowListPermissions';
import { useEnabledRolesFromAllowListState } from '../../hooks/AllowListPermissions/useEnabledRolesFromAllowListState';

export interface AllowListPermissionsTabProps {
  collectionName: string;
}

export const AllowListPermissions: React.FC<AllowListPermissionsTabProps> = ({
  collectionName,
}) => {
  const {
    allAvailableRoles,
    newRoles,
    setNewRoles,
    enabledRoles,
    setEnabledRoles,
  } = useEnabledRolesFromAllowListState(collectionName);

  const { setRoleToAllowListPermission } =
    useSetRoleToAllowListPermission(collectionName);

  const handleToggle = (roleName: string, index: number) => {
    let newEnabledRoles = [];
    // add roleName to enabledRoles, remove duplicates
    if (enabledRoles.includes(roleName)) {
      newEnabledRoles = Array.from(
        new Set(enabledRoles.filter(role => role !== roleName))
      );
      setEnabledRoles(newEnabledRoles);
    } else {
      newEnabledRoles = Array.from(new Set([...enabledRoles, roleName]));
      // remove enabled role from newRoles
      setNewRoles(
        newRoles.length > 1
          ? newRoles.filter(role => role !== newRoles[index])
          : newRoles
      );
      setEnabledRoles(newEnabledRoles);
    }

    setRoleToAllowListPermission(newEnabledRoles);
  };

  const handleNewRole = (value: string, index: number) => {
    const newAddedRoles = [...newRoles];
    newAddedRoles[index] = value;

    // if last item is not an empty string, add empty string as last index
    if (newAddedRoles[newAddedRoles.length - 1] !== '') {
      newAddedRoles.push('');
    }
    // drop last one
    if (newAddedRoles[newAddedRoles.length - 2] === '') {
      newAddedRoles.pop();
    }

    setNewRoles(newAddedRoles);
  };

  return (
    <>
      <div className="p-md">
        <div className="overflow-x-auto border border-gray-300 rounded mb-md">
          <table className="min-w-full divide-y divide-gray-300 text-left">
            <thead>
              <tr className="divide-x divide-gray-300">
                <th className="w-0 bg-gray-50 px-sm py-xs text-sm font-semibold text-muted uppercase tracking-wider">
                  Role
                </th>
                <th className="text-center bg-gray-50 px-sm py-xs text-sm font-semibold text-muted uppercase tracking-wider">
                  Access
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-300">
              <tr className="group divide-x divide-gray-300">
                <td className="w-0 bg-gray-50 p-sm font-semibold text-muted">
                  <div className="flex items-center">
                    <label className="flex items-center">admin</label>
                  </div>
                </td>
                <td className="text-center p-sm whitespace-nowrap cursor-not-allowed">
                  <FaCheck className="text-green-600" />
                </td>
              </tr>
              {allAvailableRoles.map((roleName, index) => (
                <tr className="divide-x divide-gray-300">
                  <td className="w-0 bg-gray-50 p-sm font-semibold text-muted">
                    <div className="flex items-center">
                      <label>{roleName}</label>
                    </div>
                  </td>
                  <td className="group relative text-center p-sm whitespace-nowrap cursor-pointer">
                    <Switch
                      checked={enabledRoles?.includes(roleName)}
                      onCheckedChange={() => handleToggle(roleName, index)}
                    />
                  </td>
                </tr>
              ))}
              {newRoles.map((newRole, index) => (
                <tr className="divide-x divide-gray-300">
                  <td className="w-0 bg-gray-50 p-2 font-semibold text-muted">
                    <input
                      x-model="newRole"
                      type="text"
                      className="block w-full h-input min-w-max shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                      placeholder="Create New Role..."
                      value={newRole}
                      onChange={e => handleNewRole(e.target.value, index)}
                    />
                  </td>
                  <td className="group relative text-center p-sm whitespace-nowrap cursor-pointer">
                    <Switch
                      checked={enabledRoles.includes(newRole)}
                      onCheckedChange={
                        newRole !== ''
                          ? () => handleToggle(newRole, index)
                          : () => {}
                      }
                    />
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </>
  );
};
