import React from 'react';
import { Flex, Strong } from '@radix-ui/themes';
import { TableMachine } from '../hooks';
import {
  Checkbox,
  IconButton,
  IconButtonProps,
  IconTooltip,
  Input,
  Table,
  PermissionsIcon,
} from '@hasura/shared/ui';
import { AccessType } from '@hasura/shared/types';

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
      <Table.ColumnHeaderCell align="center">
        <Input
          ref={inputRef}
          value={state.context.newRoleName}
          aria-label="create-new-role"
          placeholder="Create new role..."
          onChange={(e) => {
            send({ type: 'NEW_ROLE_NAME', newRoleName: e.target.value });
          }}
        />
      </Table.ColumnHeaderCell>
    );
  }

  return (
    <Table.ColumnHeaderCell align="center">
      <Flex align="center">
        <Checkbox
          id={roleName}
          value={isSelected}
          onChange={() => {
            send({ type: 'BULK_OPEN', roleName });
          }}
          disabled={!isSelectable || !!disabled}
        >
          <Strong>{roleName}</Strong>
        </Checkbox>
      </Flex>
    </Table.ColumnHeaderCell>
  );
};

export interface EditableCellProps extends IconButtonProps {
  access: AccessType;
  isEditable: boolean;
  isCurrentEdit: boolean;
  testId?: string;
  tooltip?: React.ReactNode;
}

export const PermissionAccessCell: React.FC<EditableCellProps> = ({
  access,
  isEditable,
  isCurrentEdit,
  testId,
  tooltip,
  ...rest
}) => {
  if (!isEditable) {
    return (
      <Table.Cell align="center" className="p-0">
        <Flex align="center" justify="center" gap="1" className="h-full">
          <PermissionsIcon type={access} />
          {tooltip ? <IconTooltip message={tooltip} /> : null}
        </Flex>
      </Table.Cell>
    );
  }

  return (
    <Table.Cell className="p-0!">
      <IconButton
        className="w-full! h-full! flex! justify-center content-center m-0! p-0!"
        variant={isCurrentEdit ? 'soft' : 'ghost'}
        radius="none"
        data-testid={testId}
        type="submit"
        color={'indigo'}
        {...rest}
      >
        <PermissionsIcon type={access} />
      </IconButton>
    </Table.Cell>
  );
};
