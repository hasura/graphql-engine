import React from 'react';
import { Story, Meta } from '@storybook/react';
import { within, userEvent, screen, waitFor } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { handlers } from '../../../../mocks/metadata.mock';
import {
  AllowListPermissions,
  AllowListPermissionsTabProps,
} from './AllowListPermissions';

export default {
  title: 'Features/Allow List/Allow List Permissions',
  component: AllowListPermissions,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta;

export const Default: Story<AllowListPermissionsTabProps> = args => {
  return <AllowListPermissions {...args} />;
};

Default.args = {
  collectionName: 'allowed-queries',
};

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await waitFor(() => {
    expect(screen.queryByText('admin')).toBeInTheDocument();
  });

  await expect(screen.queryByText('user')).toBeInTheDocument();

  // toggle the user permission and check the success notification
  await userEvent.click(canvas.getByTestId('user'));
  await expect(
    await canvas.findByText(`Allow list permissions updated`)
  ).toBeInTheDocument();

  // Add new role
  await userEvent.type(
    canvas.getByPlaceholderText('Create New Role...'),
    'role'
  );

  // toggle new role and check the success notification
  await userEvent.click(canvas.getByTestId('role'));
  await expect(
    await canvas.findByText(`Allow list permissions updated`)
  ).toBeInTheDocument();
};
