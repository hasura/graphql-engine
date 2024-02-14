import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import * as z from 'zod';

import { SimpleForm } from '../../../../new-components/Form';

import {
  ClonePermissionsSection,
  ClonePermissionsSectionProps,
} from './ClonePermissions';

export default {
  component: ClonePermissionsSection,
  decorators: [
    (StoryComponent: React.FC) => (
      <SimpleForm schema={z.any()} onSubmit={() => {}} className="p-4">
        <StoryComponent />
      </SimpleForm>
    ),
  ],
} as Meta;

export const Default: StoryObj<ClonePermissionsSectionProps> = {
  args: {
    tables: [
      {
        name: 'users',
        schema: 'public',
      },
    ],
    supportedQueryTypes: ['insert', 'select', 'update', 'delete'],
    roles: ['one', 'two'],
  },

  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },
};

export const Showcase: StoryObj<ClonePermissionsSectionProps> = {
  args: {
    ...Default.args,
    defaultOpen: true,
  },
};
