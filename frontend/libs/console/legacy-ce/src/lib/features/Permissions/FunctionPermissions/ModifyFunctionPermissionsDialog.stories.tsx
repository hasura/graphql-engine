import { Meta, StoryObj } from '@storybook/react';
import { expect } from '@storybook/jest';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ModifyFunctionPermissionsDialog } from './ModifyFunctionPermissionsDialog';
import { handlers } from './mocks';
import { action } from '@storybook/addon-actions';
import { userEvent, waitFor, within } from '@storybook/testing-library';

export default {
  component: ModifyFunctionPermissionsDialog,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Basic: StoryObj<typeof ModifyFunctionPermissionsDialog> = {
  name: '🧰 Basic',
  args: {
    dataSourceName: 'Test Snowflake Hasura',
    qualifiedFunction: ['SEARCH_ALBUMS_BY_TITLE'],
    onClose: action('onClose'),
  },
};

export const Test: StoryObj<typeof ModifyFunctionPermissionsDialog> = {
  ...Basic,
  name: '🧪 Test',
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(
      await canvas.findByText('Edit Function Permissions')
    ).toBeInTheDocument();

    const toggleDev: HTMLButtonElement = canvas.getByTestId(
      'toggle-permission-the_dev'
    );

    await userEvent.click(toggleDev);

    await waitFor(() =>
      expect(toggleDev).toHaveAttribute('data-state', 'unchecked')
    );
    await expect(
      await canvas.findByText('Permissions saved successfully!')
    ).toBeInTheDocument();

    const toggleUser: HTMLButtonElement = canvas.getByTestId(
      'toggle-permission-the_user'
    );

    await userEvent.click(toggleUser);

    await waitFor(() =>
      expect(toggleUser).toHaveAttribute('data-state', 'checked')
    );
    await expect(await canvas.findByText('Error!')).toBeInTheDocument();
  },
};
