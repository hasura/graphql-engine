import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { z } from 'zod';
import { SimpleForm } from '../../../../new-components/Form';
import { useTableMachine } from '../hooks/useTableMachine';

import {
  EditableCell,
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
  render: args => {
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
  render: args => {
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
  render: args => (
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
          <EditableCell {...args} />
          <EditableCell {...{ ...args, isCurrentEdit: true }} />
          <EditableCell {...{ ...args, access: 'noAccess' }} />
          <EditableCell {...{ ...args, access: 'fullAccess' }} />
        </tr>
      </tbody>
    </table>
  ),

  args: {
    isEditable: true,
  },
};
