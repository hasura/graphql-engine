import React from 'react';
import { Story, Meta } from '@storybook/react';
import * as z from 'zod';

import { Form } from '@/new-components/Form';

import {
  ClonePermissionsSection,
  ClonePermissionsSectionProps,
} from './ClonePermissions';

export default {
  title: 'Features/Permissions Form/Components/Clone Permissions',
  component: ClonePermissionsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <Form schema={z.any()} onSubmit={() => {}}>
        {() => <StoryComponent />}
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
