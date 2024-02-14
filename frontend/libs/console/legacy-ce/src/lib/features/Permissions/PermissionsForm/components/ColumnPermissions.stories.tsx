import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { z } from 'zod';
import { SimpleForm } from '../../../../new-components/Form';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';

import {
  ColumnPermissionsSection,
  ColumnPermissionsSectionProps,
} from './ColumnPermissions';

const schema = z.object({ columns: z.record(z.optional(z.boolean())) });

export default {
  component: ColumnPermissionsSection,
  decorators: [
    ReactQueryDecorator(),
    (StoryComponent: React.FC) => (
      <SimpleForm
        schema={schema}
        onSubmit={() => {}}
        options={{
          defaultValues: {
            columns: { id: false, name: false, description: false },
          },
        }}
        className="p-4"
      >
        <StoryComponent />
      </SimpleForm>
    ),
  ],
  parameters: {
    // Disable storybook for playground stories
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const columns = ['id', 'name', 'description'];

export const Insert: StoryObj<ColumnPermissionsSectionProps> = {
  args: {
    roleName: 'two',
    queryType: 'insert',
    columns,
  },
};

export const Select: StoryObj<ColumnPermissionsSectionProps> = {
  args: {
    ...Insert.args,
    queryType: 'select',
  },
};

export const Update: StoryObj<ColumnPermissionsSectionProps> = {
  args: {
    ...Insert.args,
    queryType: 'update',
  },
};

export const Delete: StoryObj<ColumnPermissionsSectionProps> = {
  args: {
    ...Insert.args,
    queryType: 'delete',
  },
};

export const PartiallySelected: StoryObj<ColumnPermissionsSectionProps> = {
  args: {
    ...Insert.args,
    queryType: 'insert',
  },

  decorators: [
    (S: React.FC) => (
      <SimpleForm
        schema={schema}
        onSubmit={() => {}}
        options={{
          defaultValues: {
            columns: { id: true, name: false, description: true },
          },
        }}
        className="p-4"
      >
        <S />
      </SimpleForm>
    ),
  ],
};

export const AllSelected: StoryObj<ColumnPermissionsSectionProps> = {
  args: {
    ...Insert.args,
  },

  decorators: [
    (S: React.FC) => (
      <SimpleForm
        schema={schema}
        onSubmit={() => {}}
        options={{
          defaultValues: {
            columns: { id: true, name: true, description: true },
          },
        }}
        className="p-4"
      >
        <S />
      </SimpleForm>
    ),
  ],
};

export const Showcase: StoryObj<ColumnPermissionsSectionProps> = {
  args: {
    ...Insert.args,
  },

  parameters: {
    chromatic: { disableSnapshot: false },
  },
};
