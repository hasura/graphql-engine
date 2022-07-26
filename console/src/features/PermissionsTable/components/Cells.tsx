import React from 'react';
import { TableMachine } from '../hooks';
import { PermissionsIcon } from './PermissionsIcons';

export interface InputCellProps extends React.ComponentProps<'input'> {
  roleName: string;
  isNewRole: boolean;
  isSelectable: boolean;
  isSelected: boolean;
  machine: ReturnType<TableMachine>;
}

export const InputCell: React.FC<InputCellProps> = ({
  roleName,
  isNewRole,
  isSelectable,
  isSelected,
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
    <td className="w-0 bg-gray-50 p-md font-semibold text-muted">
      <div className="flex items-center">
        <input
          id={roleName}
          type="checkbox"
          value={roleName}
          checked={isSelected}
          className={`rounded shadow-sm border border-gray-300 hover:border-gray-400 focus:ring-yellow-400 m-0 ${
            !isSelectable && 'cursor-not-allowed'
          }`}
          disabled={!isSelectable}
          onChange={() => {
            send({ type: 'BULK_OPEN', roleName });
          }}
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
      <td className="p-md whitespace-nowrap text-center">
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
