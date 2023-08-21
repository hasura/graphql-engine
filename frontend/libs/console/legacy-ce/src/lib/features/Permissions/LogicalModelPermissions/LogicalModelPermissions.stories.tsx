import { Meta, StoryObj } from '@storybook/react';

import { LogicalModelPermissions } from './LogicalModelPermissions';
import { comparators } from '../PermissionsForm/components/RowPermissionsBuilder/components/__tests__/fixtures/comparators';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ComponentProps } from 'react';

export default {
  component: LogicalModelPermissions,
} as Meta;

type Story = StoryObj<typeof LogicalModelPermissions>;

const noPermissionArgs: Partial<
  ComponentProps<typeof LogicalModelPermissions>
> = {
  logicalModels: [
    {
      source: {
        name: 'default',
        kind: 'postgres',
        configuration: {},
        tables: [],
      },
      fields: [
        {
          name: 'one',
          type: {
            scalar: 'text',
            nullable: false,
          },
        },
        {
          name: 'two',
          type: {
            scalar: 'text',
            nullable: false,
          },
        },
      ],
      name: 'hello_world',
    },
  ],
  logicalModelName: 'hello_world',
  comparators,
  roles: [],
};

const existingPermissionArgs: Partial<
  ComponentProps<typeof LogicalModelPermissions>
> = {
  logicalModels: [
    {
      source: {
        name: 'default',
        kind: 'postgres',
        configuration: {},
        tables: [],
      },
      fields: [
        {
          name: 'one',
          type: {
            scalar: 'text',
            nullable: false,
          },
        },
        {
          name: 'two',
          type: {
            scalar: 'text',
            nullable: false,
          },
        },
      ],
      name: 'hello_world',
      select_permissions: [
        {
          permission: {
            columns: ['one'],
            filter: { one: { _eq: 'eqone' } },
          },
          role: 'user',
        },
      ],
    },
  ],
  logicalModelName: 'hello_world',
  comparators,
  roles: ['user'],
};

export const NewPermission: Story = {
  args: noPermissionArgs,
  play: async ({ canvasElement }) => {
    // Should allow writing new role name and then clicking select should open form
    const canvas = within(canvasElement);
    await userEvent.type(canvas.getByTestId('new-role-input'), 'reader');
    await userEvent.click(canvas.getByTestId('reader-select-permissions-cell'));
    await expect(canvas.getByTestId('permissions-form')).toBeInTheDocument();
  },
};

export const ExistingPermission: Story = {
  args: existingPermissionArgs,
  play: async ({ canvasElement, args: { onSave } }) => {
    // Clicking select on existing permission should open form with correct role (user) and action (select)
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('user-select-permissions-cell'));
    await expect(canvas.getByTestId('permissions-form')).toBeInTheDocument();
    await expect((await canvas.findByTestId('role-pill')).textContent).toBe(
      'user'
    );
    await expect((await canvas.findByTestId('action-pill')).textContent).toBe(
      'select'
    );

    // It should send correct existing permission (`{"one":{"_eq":"eqone"}}`) when clicking save
    await userEvent.click(canvas.getByTestId('save-permissions-button'));
    await expect(onSave).toHaveBeenCalledWith({
      roleName: 'user',
      filter: {
        one: {
          _eq: 'eqone',
        },
      },
      columns: ['one'],
      action: 'select',
      isNew: false,
      source: 'default',
    });
  },
};

export const UpdatingPermissions: Story = {
  args: existingPermissionArgs,
  play: async ({ canvasElement, args: { onSave } }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('user-select-permissions-cell'));

    // Clicking "Without any checks" should set permission to `{}`
    await userEvent.click(canvas.getByTestId('without-filter'));
    // Clicking `two` should select that column
    await userEvent.click(canvas.getByTestId('column-two-checkbox'));

    await userEvent.click(canvas.getByTestId('save-permissions-button'));
    await expect(onSave).toHaveBeenCalledWith({
      roleName: 'user',
      filter: {},
      columns: ['one', 'two'],
      action: 'select',
      isNew: false,
      source: 'default',
    });
  },
};

export const ToggleAllColumns: Story = {
  args: existingPermissionArgs,
  play: async ({ canvasElement, args: { onSave } }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('user-select-permissions-cell'));

    await userEvent.click(canvas.getByTestId('toggle-all-columns'));

    await userEvent.click(canvas.getByTestId('save-permissions-button'));
    await expect(onSave).toHaveBeenCalledWith({
      roleName: 'user',
      filter: { one: { _eq: 'eqone' } },
      columns: ['one', 'two'],
      action: 'select',
      isNew: false,
      source: 'default',
    });
  },
};

export const UntoggleAllColumns: Story = {
  args: existingPermissionArgs,
  play: async ({ canvasElement, args: { onSave } }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId('user-select-permissions-cell'));

    await userEvent.click(canvas.getByTestId('toggle-all-columns'));
    await userEvent.click(canvas.getByTestId('toggle-all-columns'));

    await userEvent.click(canvas.getByTestId('save-permissions-button'));
    await expect(onSave).toHaveBeenCalledWith({
      roleName: 'user',
      filter: { one: { _eq: 'eqone' } },
      columns: [],
      action: 'select',
      isNew: false,
      source: 'default',
    });
  },
};
