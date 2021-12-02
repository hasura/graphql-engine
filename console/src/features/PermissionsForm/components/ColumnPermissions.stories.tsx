import React from 'react';
import { Story, Meta } from '@storybook/react';
import { z } from 'zod';
import { Form } from '@/new-components/Form';

import {
  ColumnPermissionsSection,
  ColumnPermissionsSectionProps,
} from './ColumnPermissions';

const schema = z.object({ columns: z.record(z.optional(z.boolean())) });

export default {
  title: 'Permissions Form/Components/Column Section',
  component: ColumnPermissionsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <Form
        schema={schema}
        onSubmit={() => {}}
        options={{
          defaultValues: {
            columns: { id: false, name: false, description: false },
          },
        }}
      >
        {() => <StoryComponent />}
      </Form>
    ),
  ],
  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const columns = ['id', 'name', 'description'];

export const Insert: Story<ColumnPermissionsSectionProps> = args => (
  <ColumnPermissionsSection {...args} />
);
Insert.args = {
  roleName: 'two',
  queryType: 'insert',
  columns,
  defaultOpen: true,
};

export const Select: Story<ColumnPermissionsSectionProps> = args => (
  <ColumnPermissionsSection {...args} />
);
Select.args = {
  ...Insert.args,
  queryType: 'select',
};

export const Update: Story<ColumnPermissionsSectionProps> = args => (
  <ColumnPermissionsSection {...args} />
);
Update.args = {
  ...Insert.args,
  queryType: 'update',
};

export const Delete: Story<ColumnPermissionsSectionProps> = args => (
  <ColumnPermissionsSection {...args} />
);
Delete.args = {
  ...Insert.args,
  queryType: 'delete',
};

export const PartiallySelected: Story<ColumnPermissionsSectionProps> = args => (
  <ColumnPermissionsSection {...args} />
);
PartiallySelected.args = {
  ...Insert.args,
  queryType: 'insert',
};
PartiallySelected.decorators = [
  (S: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{
        defaultValues: {
          columns: { id: true, name: false, description: true },
        },
      }}
    >
      {() => <S />}
    </Form>
  ),
];

export const AllSelected: Story<ColumnPermissionsSectionProps> = args => (
  <ColumnPermissionsSection {...args} />
);
AllSelected.args = {
  ...Insert.args,
};
AllSelected.decorators = [
  (S: React.FC) => (
    <Form
      schema={schema}
      onSubmit={() => {}}
      options={{
        defaultValues: {
          columns: { id: true, name: true, description: true },
        },
      }}
    >
      {() => <S />}
    </Form>
  ),
];

export const Showcase: Story<ColumnPermissionsSectionProps> = args => (
  <ColumnPermissionsSection {...args} />
);
Showcase.args = {
  ...Insert.args,
};
Showcase.parameters = {
  chromatic: { disableSnapshot: false },
};
