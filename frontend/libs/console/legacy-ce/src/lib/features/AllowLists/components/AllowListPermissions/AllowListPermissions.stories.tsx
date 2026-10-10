import { expect, screen, userEvent, waitFor, within } from 'storybook/test';
import { Meta, StoryObj } from '@storybook/react-webpack5';
import { ReactQueryDecorator, handlers } from '@hasura/shared/testing';
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

export const Default: StoryObj<AllowListPermissionsTabProps> = {
  render: (args) => {
    return <AllowListPermissions {...args} />;
  },

  args: {
    collectionName: 'allowed-queries',
  },
};

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await waitFor(async () => {
    await expect(screen.queryByText('admin')).toBeInTheDocument();
  });

  await expect(screen.queryByText('user')).toBeInTheDocument();

  // toggle the user permission and check the success notification
  await userEvent.click(canvas.getByTestId('user'));
  await expect(
    await canvas.findByText(`Allow list permissions updated`),
  ).toBeInTheDocument();

  // Add new role
  await userEvent.type(
    canvas.getByPlaceholderText('Create New Role...'),
    'role',
  );

  // toggle new role and check the success notification
  await userEvent.click(canvas.getByTestId('role'));
  await expect(
    await canvas.findByText(`Allow list permissions updated`),
  ).toBeInTheDocument();
};
