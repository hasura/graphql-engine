import React from 'react';
import {
  QueryRootPermissionType,
  SubscriptionRootPermissionType,
  PermissionRootType,
} from './types';

type Props = {
  permission: QueryRootPermissionType | SubscriptionRootPermissionType;
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
        <input
          type="checkbox"
          className="legacy-input-fix disabled:bg-gray-100 disabled:cursor-not-allowed"
          checked={checked}
          value={permission}
          onChange={() => onPermissionChange(permission)}
          disabled={disabled}
        />
        <i>{permission}</i>
      </label>
    </div>
  </div>
);
