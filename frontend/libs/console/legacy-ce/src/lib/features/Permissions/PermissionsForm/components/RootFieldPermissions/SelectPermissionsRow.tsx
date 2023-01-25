import React from 'react';
import { SelectPermissionFields } from './SelectPermissionFields';
import { SelectPermissionSectionHeader } from './SelectPermissionSectionHeader';
import { getPermissionCheckboxState } from './utils';
import { RootKeyValues } from './RootFieldPermissions';
import {
  PermissionRootType,
  PermissionRootTypes,
  CombinedPermissionRootTypes,
} from './types';

type Props = {
  onToggleAll: () => void;
  permissionFields: PermissionRootTypes;
  currentPermissions: CombinedPermissionRootTypes;
  onUpdate: (
    key: RootKeyValues,
    value: PermissionRootType,
    permissions: CombinedPermissionRootTypes
  ) => void;
  description: React.ReactElement;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  permissionType: RootKeyValues;
  isSubscriptionStreamingEnabled: boolean;
};

export const SelectPermissionsRow: React.FC<Props> = ({
  onToggleAll,
  permissionFields,
  currentPermissions,
  onUpdate,
  description,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  permissionType,
  isSubscriptionStreamingEnabled,
}) => {
  return (
    <div>
      <SelectPermissionSectionHeader
        onToggle={onToggleAll}
        text={description}
      />
      <div className="px-md flex">
        {permissionFields?.map(permission => {
          const permissionCheckboxState = getPermissionCheckboxState({
            permission,
            hasEnabledAggregations,
            hasSelectedPrimaryKeys,
            isSubscriptionStreamingEnabled,
            rootPermissions: currentPermissions,
          });

          return (
            <SelectPermissionFields
              key={permission}
              title={
                currentPermissions === null
                  ? 'Set row permissions first'
                  : permissionCheckboxState.title
              }
              disabled={permissionCheckboxState.disabled}
              checked={permissionCheckboxState.checked}
              onPermissionChange={(value: PermissionRootType) => {
                onUpdate(permissionType, value, currentPermissions);
              }}
              permission={permission}
            />
          );
        })}
      </div>
    </div>
  );
};
