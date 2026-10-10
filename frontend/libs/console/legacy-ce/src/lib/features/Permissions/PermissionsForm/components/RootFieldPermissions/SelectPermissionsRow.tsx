import React from 'react';
import { Flex } from '@radix-ui/themes';
import { SelectPermissionSectionHeader } from './SelectPermissionSectionHeader';
import { getPermissionCheckboxState } from './utils';
import { RootKeyValues } from './RootFieldPermissions';
import { PermissionRootTypes } from './types';
import { CheckboxesField } from '@hasura/shared/ui';

type Props = {
  onToggleAll: () => void;
  permissionFields: PermissionRootTypes;
  description: React.ReactElement<any>;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  name: RootKeyValues;
  isSubscriptionStreamingEnabled: boolean;
};

export const SelectPermissionsRow: React.FC<Props> = ({
  name,
  onToggleAll,
  permissionFields,
  description,
  hasEnabledAggregations,
  hasSelectedPrimaryKeys,
  isSubscriptionStreamingEnabled,
}) => {
  return (
    <div className="my-4">
      <SelectPermissionSectionHeader
        onToggle={onToggleAll}
        text={description}
      />
      <Flex className="mt-2">
        <CheckboxesField
          name={name}
          orientation="horizontal"
          options={permissionFields.map((permission) => ({
            label: permission,
            value: permission,
            ...getPermissionCheckboxState({
              permission,
              hasEnabledAggregations,
              hasSelectedPrimaryKeys,
              isSubscriptionStreamingEnabled,
            }),
          }))}
        />
      </Flex>
    </div>
  );
};
