import React from 'react';
import { useFormContext } from 'react-hook-form';
import { PermissionsIcon } from './PermissionsIcons';

export interface InputCellProps extends React.ComponentProps<'input'> {
  roleName: string;
  isNewRole: boolean;
  isSelectable: boolean;
}

export const InputCell: React.FC<InputCellProps> = ({
  roleName,
  isNewRole,
  isSelectable,
}) => {
  const { register } = useFormContext();

  if (isNewRole) {
    return (
      <td className="w-0 bg-gray-50 p-sm font-semibold text-muted">
        <input
          aria-label="create-new-role"
          placeholder="Create new role..."
          className="block w-64 h-input px-md shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          {...register('newRoleName')}
        />
      </td>
    );
  }

  return (
    <td className="w-0 bg-gray-50 p-md font-semibold text-muted">
      <div className="flex items-center">
        <input
          id={roleName}
          type="checkbox"
          value={roleName}
          defaultChecked={false}
          className={`rounded shadow-sm border border-gray-300 hover:border-gray-400 focus:ring-yellow-400 ${
            !isSelectable && 'cursor-not-allowed'
          }`}
          disabled={!isSelectable}
          {...register('bulkSelected')}
        />
        <label className="flex items-center ml-sm" htmlFor={roleName}>
          {roleName}
        </label>
      </div>
    </td>
  );
};

export interface EditableCellProps extends React.ComponentProps<'button'> {
  access: 'fullAccess' | 'partialAccess' | 'noAccess';
  isEditable: boolean;
  isCurrentEdit: boolean;
}

export const EditableCell: React.FC<EditableCellProps> = ({
  access,
  isEditable,
  isCurrentEdit,
  ...rest
}) => {
  if (!isEditable) {
    return (
      <td className="cursor-pointer p-md whitespace-nowrap text-center">
        <PermissionsIcon type={access} selected={isCurrentEdit} />
      </td>
    );
  }

  return (
    <td>
      <button
        type="submit"
        className={`cursor-pointer h-20 border-none w-full whitespace-nowrap text-center ${
          isCurrentEdit ? 'bg-secondary' : 'hover:bg-indigo-50'
        }`}
        {...rest}
      >
        <PermissionsIcon type={access} selected={isCurrentEdit} />
      </button>
    </td>
  );
};
