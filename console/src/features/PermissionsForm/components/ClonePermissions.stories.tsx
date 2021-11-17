import React from 'react';
import { Story, Meta } from '@storybook/react';
import * as z from 'zod';

import { Form } from '@/new-components/Form';

import {
  ClonePermissionsSection,
  ClonePermissionsSectionProps,
} from './ClonePermissions';

export default {
  title: 'Permissions Form/Components/Clone Permissions',
  component: ClonePermissionsSection,
  decorators: [
    (S: React.FC) => (
      <Form schema={z.any()} onSubmit={() => {}}>
        {() => <S />}
      </Form>
    ),
  ],
} as Meta;

export const Default: Story<ClonePermissionsSectionProps> = args => (
  <ClonePermissionsSection {...args} />
);
Default.args = {
  tables: ['users', 'public'],
  supportedQueryTypes: ['insert', 'select', 'update', 'delete'],
  roles: ['one', 'two'],
};

export const DefaultOpen: Story<ClonePermissionsSectionProps> = args => (
  <ClonePermissionsSection {...args} />
);
DefaultOpen.args = {
  ...Default.args,
  defaultOpen: true,
};
