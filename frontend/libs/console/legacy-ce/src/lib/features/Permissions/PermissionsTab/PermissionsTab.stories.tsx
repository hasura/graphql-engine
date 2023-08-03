import { StoryObj, Meta } from '@storybook/react';

import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

import { PermissionsTab, PermissionsTabProps } from './PermissionsTab';
import { handlers } from '../PermissionsForm/mocks/handlers.mock';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { userEvent } from '@storybook/testing-library';

export default {
  component: PermissionsTab,
  decorators: [ReactQueryDecorator()],
} as Meta;

export const Basic: StoryObj<PermissionsTabProps> = {
  args: {
    dataSourceName: 'Lite',
    table: ['Artist'],
  },

  parameters: {
    msw: handlers(),
  },
};

export const UpdatePermissions: StoryObj<PermissionsTabProps> = {
  args: {
    dataSourceName: 'Lite',
    table: ['Artist'],
  },

  parameters: {
    msw: handlers(),
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // Type "viewer" into input with aria-label "create-new-role"
    await userEvent.type(
      await canvas.findByLabelText('create-new-role'),
      'viewer'
    );
    // Click permission-table-button-newRole-select
    await userEvent.click(
      await canvas.findByTestId('permission-table-button-newRole-select')
    );

    // Click custom-check
    await userEvent.click(await canvas.findByTestId('custom-check'));

    await userEvent.selectOptions(
      await canvas.findByTestId('-operator'),
      '_and'
    );

    await userEvent.selectOptions(
      await canvas.findByTestId('_and.1-operator'),
      'ArtistId'
    );

    // click _and.1.ArtistId._eq-value-input-x-hasura-user-id
    await userEvent.click(
      await canvas.findByTestId(
        '_and.1.ArtistId._eq-value-input-x-hasura-user-id'
      )
    );

    // Click submit button
    await userEvent.click(await canvas.findByTestId('permissions-form-submit'));

    await expect(
      await canvas.findByText('Permissions saved successfully!')
    ).toBeInTheDocument();
  },
};
