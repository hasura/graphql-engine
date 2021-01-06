import React, { FC } from 'react';

interface PermissionCheckboxProps {
  fieldName: string;
  checked?: boolean;
  onToggle: () => void;
  disabled?: boolean;
  title?: string;
  labelClassName?: string;
}

const PermissionCheckbox: FC<PermissionCheckboxProps> = props => {
  const {
    fieldName,
    checked,
    onToggle,
    disabled,
    title,
    labelClassName,
  } = props;
  return (
    <div key={fieldName}>
      <div className="checkbox">
        <label className={labelClassName}>
          <input
            type="checkbox"
            checked={checked}
            value={fieldName}
            onChange={() => onToggle()}
            disabled={disabled}
            title={title}
          />
          {fieldName}
        </label>
      </div>
    </div>
  );
};

export default PermissionCheckbox;
