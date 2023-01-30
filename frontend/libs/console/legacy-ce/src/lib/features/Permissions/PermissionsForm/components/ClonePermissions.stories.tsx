import React from 'react';
import { Meta, Story } from '@storybook/react';
import * as z from 'zod';

import { SimpleForm } from '@/new-components/Form';

import {
  ClonePermissionsSection,
  ClonePermissionsSectionProps,
} from './ClonePermissions';

export default {
  title: 'Features/Permissions/Form/Clone Permissions',
  component: ClonePermissionsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <SimpleForm schema={z.any()} onSubmit={() => {}} className="p-4">
        <StoryComponent />
      </SimpleForm>
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
Default.parameters = {
  // Disable storybook for playground stories
  chromatic: { disableSnapshot: true },
};

export const Showcase: Story<ClonePermissionsSectionProps> = args => (
  <ClonePermissionsSection {...args} />
);
Showcase.args = {
  ...Default.args,
  defaultOpen: true,
};
