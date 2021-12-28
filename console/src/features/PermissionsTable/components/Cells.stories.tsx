import React from 'react';
import { Story, Meta } from '@storybook/react';
import { z } from 'zod';

import { Form } from '@/new-components/Form';
import {
  InputCell,
  InputCellProps,
  EditableCell,
  EditableCellProps,
} from './Cells';

export default {
  title: 'Permissions Table/Components/Cells',
  component: InputCell,
  decorators: [
    (StoryComponent: React.FC) => (
      <Form schema={z.any()} onSubmit={() => {}}>
        {() => <StoryComponent />}
      </Form>
    ),
  ],
  parameters: { chromatic: { disableSnapshot: true } },
} as Meta;

export const InputCellComponent: Story<InputCellProps> = args => (
  <InputCell {...args} />
);
InputCellComponent.args = {
  roleName: 'User',
};

export const InputCellComponentNewRole: Story<InputCellProps> = args => (
  <InputCell {...args} />
);
InputCellComponentNewRole.args = {
  roleName: '',
  isNewRole: true,
};

export const EditableCellComponent: Story<EditableCellProps> = args => (
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
);
EditableCellComponent.args = {
  isEditable: true,
};
