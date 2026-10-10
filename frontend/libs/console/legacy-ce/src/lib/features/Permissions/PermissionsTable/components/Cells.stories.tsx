import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { z } from 'zod';
import { SimpleForm } from '@hasura/shared/ui';
import { useTableMachine } from '../hooks/useTableMachine';

import {
  PermissionAccessCell,
  EditableCellProps,
  InputCell,
  InputCellProps,
} from './Cells';

export default {
  component: InputCell,
  decorators: [
    (StoryComponent: React.FC) => (
      <SimpleForm schema={z.any()} onSubmit={() => {}}>
        <StoryComponent />
      </SimpleForm>
    ),
  ],
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

export const InputCellComponent: StoryObj<InputCellProps> = {
  render: (args) => {
    const machine = useTableMachine();

    return <InputCell {...args} machine={machine} />;
  },

  args: {
    roleName: 'User',
    isNewRole: true,
    isSelectable: true,
    isSelected: true,
  },
};

export const InputCellComponentNewRole: StoryObj<InputCellProps> = {
  render: (args) => {
    const machine = useTableMachine();

    return <InputCell {...args} machine={machine} />;
  },

  args: {
    roleName: '',
    isNewRole: true,
    isSelectable: true,
    isSelected: true,
  },
};

export const EditableCellComponent: StoryObj<EditableCellProps> = {
  render: (args) => (
    <table>
      <thead>
        <tr>
          <th className="px-4">Default</th>
          <th className="px-4">Current Edit</th>
          <th className="px-4">No Access</th>
          <th className="px-4">Full Access</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <PermissionAccessCell {...args} />
          <PermissionAccessCell {...{ ...args, isCurrentEdit: true }} />
          <PermissionAccessCell {...{ ...args, access: 'noAccess' }} />
          <PermissionAccessCell {...{ ...args, access: 'fullAccess' }} />
        </tr>
      </tbody>
    </table>
  ),

  args: {
    isEditable: true,
  },
};
