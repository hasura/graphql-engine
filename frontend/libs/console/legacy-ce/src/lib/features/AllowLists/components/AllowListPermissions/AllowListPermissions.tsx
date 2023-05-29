import React from 'react';
import { CgSpinner } from 'react-icons/cg';

import { Button } from '../../../../new-components/Button';
import { Switch } from '../../../../new-components/Switch';
import { MetadataSelector, useMetadata } from '../../../MetadataAPI';
import { useFireNotification } from '../../../../new-components/Notifications';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';

import { FaCheck, FaPlusCircle } from 'react-icons/fa';
import { useSetRoleToAllowListPermission } from '../../hooks/AllowListPermissions/useSetRoleToAllowListPermissions';
import { useEnabledRolesFromAllowListState } from '../../hooks/AllowListPermissions/useEnabledRolesFromAllowListState';
import { useAddToAllowList } from '../../hooks/useAddToAllowList';

export interface AllowListPermissionsTabProps {
  collectionName: string;
}

export const AllowListPermissions: React.FC<AllowListPermissionsTabProps> = ({
  collectionName,
}) => {
  const { allAvailableRoles, newRoles, setNewRoles, enabledRoles } =
    useEnabledRolesFromAllowListState(collectionName);

  const { data: isCollectionInAllowlist, isLoading } = useMetadata(
    MetadataSelector.isCollectionInAllowlist(collectionName)
  );

  const { setRoleToAllowListPermission } =
    useSetRoleToAllowListPermission(collectionName);

  const { addToAllowList, isLoading: addToAllowListLoading } =
    useAddToAllowList();

  const { fireNotification } = useFireNotification();

  const [updatingRoles, setUpdatingRoles] = React.useState<string[]>([]);

  if (isLoading) {
    return null;
  }

  const handleToggle = (roleName: string) => {
    setUpdatingRoles([...updatingRoles, roleName]);
    let newEnabledRoles: string[] = [];
    // add roleName to enabledRoles, remove duplicates
    if (enabledRoles.includes(roleName)) {
      newEnabledRoles = Array.from(
        new Set(enabledRoles.filter(role => role !== roleName))
      );
    } else {
      newEnabledRoles = Array.from(new Set([...enabledRoles, roleName]));
    }

    setRoleToAllowListPermission(newEnabledRoles, {
      onSuccess: () => {
        fireNotification({
          type: 'success',
          message: 'Allow list permissions updated',
          title: 'Success',
        });
        setUpdatingRoles(updatingRoles.filter(role => role !== roleName));
      },
      onError: e => {
        fireNotification({
          type: 'error',
          message: `Error updating allow list permissions: ${e.message}`,
          title: 'Error',
        });
        setUpdatingRoles(updatingRoles.filter(role => role !== roleName));
      },
    });
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

  if (!isCollectionInAllowlist) {
    return (
      <div className="flex flex-col items-center justify-center h-full">
        <div className="text-2xl font-bold text-gray-500">
          This collection is not in the allowlist
        </div>
        <div className="text-gray-500">
          Please add this collection to the allowlist to manage permissions
        </div>
        <Button
          icon={<FaPlusCircle />}
          mode="primary"
          className="mt-4"
          onClick={() => {
            addToAllowList(collectionName, {
              onSuccess: () => {
                fireNotification({
                  type: 'success',
                  title: 'Collection added to allowlist',
                  message: `Collection ${collectionName} has been added to the allowlist`,
                });
              },
              onError: e => {
                fireNotification({
                  type: 'error',
                  title: 'Collection not added to allowlist',
                  message: `Collection ${collectionName} could not be added to the allowlist: ${e.message}`,
                });
              },
            });
          }}
          isLoading={addToAllowListLoading}
        >
          Add to allowlist
        </Button>
      </div>
    );
  }

  return (
    <>
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
            {allAvailableRoles.map(roleName => (
              <tr className="divide-x divide-gray-300">
                <td className="w-0 bg-gray-50 p-sm font-semibold text-muted">
                  <div className="flex items-center">
                    <label>{roleName}</label>
                  </div>
                </td>
                <td className="group relative text-center p-sm whitespace-nowrap cursor-pointer">
                  {updatingRoles.length > 0 ? (
                    <CgSpinner className={`animate-spin ${'w-5 h-5'}`} />
                  ) : (
                    <Switch
                      checked={enabledRoles.includes(roleName)}
                      onCheckedChange={() => handleToggle(roleName)}
                      data-testid={roleName}
                    />
                  )}
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
                <td className="group relative text-center p-sm whitespace-nowrap cursor-pointer flex items-center justify-center">
                  {updatingRoles.length > 0 ? (
                    <CgSpinner className={`animate-spin ${'w-5 h-5'}`} />
                  ) : (
                    <Switch
                      checked={enabledRoles.includes(newRole)}
                      onCheckedChange={
                        newRole !== '' ? () => handleToggle(newRole) : () => {}
                      }
                      data-testid={newRole}
                    />
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      {enabledRoles.length === 0 && (
        <IndicatorCard>
          <p className="m-0">
            The collection is in <span className="font-bold">global mode</span>:
            all users are enabled. If you want to assign permissions in a more
            granular way, enable specific roles. If you want to disable all
            users, remove the collection from the allow list{' '}
          </p>
        </IndicatorCard>
      )}
    </>
  );
};
