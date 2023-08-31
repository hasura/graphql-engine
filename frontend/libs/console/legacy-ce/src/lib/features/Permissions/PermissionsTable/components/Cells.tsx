import React from 'react';
import { TableMachine } from '../hooks';
import { PermissionsIcon } from './PermissionsIcons';
import { Checkbox } from '../../../../new-components/Form';

export interface InputCellProps extends React.ComponentProps<'input'> {
  roleName: string;
  isNewRole: boolean;
  isSelectable: boolean;
  isSelected: boolean;
  disabled?: boolean;
  machine: ReturnType<TableMachine>;
}

export const InputCell: React.FC<InputCellProps> = ({
  roleName,
  isNewRole,
  isSelectable,
  isSelected,
  disabled,
  machine,
}) => {
  const [state, send] = machine;
  const inputRef = React.createRef<HTMLInputElement>();

  React.useEffect(() => {
    if (inputRef?.current && state.value === 'updateRoleName') {
      inputRef.current.focus();
    }
  }, [inputRef, state.value]);

  if (isNewRole) {
    return (
      <td className="w-0 bg-gray-50 p-sm font-semibold text-muted">
        <input
          ref={inputRef}
          className="block w-64 h-input px-md shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
          value={state.context.newRoleName}
          aria-label="create-new-role"
          placeholder="Create new role..."
          onChange={e => {
            send({ type: 'NEW_ROLE_NAME', newRoleName: e.target.value });
          }}
        />
      </td>
    );
  }

  return (
    <td className="w-0 bg-gray-50 p-md">
      <div className="flex items-center">
        <Checkbox
          id={roleName}
          checked={isSelected}
          onCheckedChange={() => {
            send({ type: 'BULK_OPEN', roleName });
          }}
          disabled={!isSelectable || !!disabled}
        >
          <label className="font-semibold text-muted" htmlFor={roleName}>
            {roleName}
          </label>
        </Checkbox>
      </div>
    </td>
  );
};

export interface EditableCellProps extends React.ComponentProps<'button'> {
  access: 'fullAccess' | 'partialAccess' | 'noAccess';
  isEditable: boolean;
  isCurrentEdit: boolean;
  testId: string;
}

export const EditableCell: React.FC<EditableCellProps> = ({
  access,
  isEditable,
  isCurrentEdit,
  testId,
  ...rest
}) => {
  if (!isEditable) {
    return (
      <td className="p-md whitespace-nowrap text-center cursor-not-allowed opacity-30">
        <PermissionsIcon type={access} selected={isCurrentEdit} />
      </td>
    );
  }

  return (
    <td>
      <button
        data-testid={testId}
        type="submit"
        className={`cursor-pointer h-20 border-none w-full whitespace-nowrap text-center ${
          isCurrentEdit ? 'bg-blue-100' : 'hover:bg-gray-100'
        }`}
        {...rest}
      >
        <PermissionsIcon type={access} selected={isCurrentEdit} />
      </button>
    </td>
  );
};
