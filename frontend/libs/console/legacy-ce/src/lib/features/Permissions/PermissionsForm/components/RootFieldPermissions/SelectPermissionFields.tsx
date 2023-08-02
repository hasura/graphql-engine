import React from 'react';
import { Checkbox } from '../../../../../new-components/Form';
import { PermissionRootType } from './types';

type Props = {
  permission: PermissionRootType;
  onPermissionChange: (value: PermissionRootType) => void;
  disabled?: boolean;
  checked: boolean;
  title?: string;
};

export const SelectPermissionFields: React.FC<Props> = ({
  permission,
  onPermissionChange,
  disabled = false,
  checked = true,
  title,
}) => (
  <div className="mr-sm">
    <div className="checkbox">
      <label title={title}>
        <Checkbox
          checked={checked}
          disabled={disabled}
          name={permission}
          onCheckedChange={() => onPermissionChange(permission)}
        >
          {permission}
        </Checkbox>
      </label>
    </div>
  </div>
);
