import { StoryObj, Meta } from '@storybook/react';

import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

import { PermissionsTab, PermissionsTabProps } from './PermissionsTab';
import { handlers } from '../PermissionsForm/mocks/handlers.mock';
import { within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { userEvent } from '@storybook/testing-library';

export default {
  title: 'Features/Permissions',
  component: PermissionsTab,
  decorators: [ReactQueryDecorator()],
} as Meta;

export const GDC: StoryObj<PermissionsTabProps> = {
  args: {
    dataSourceName: 'sqlite',
    table: ['Artist'],
  },

  parameters: {
    msw: handlers(),
  },
};

export const GDCNoMocks: StoryObj<PermissionsTabProps> = {
  args: {
    dataSourceName: 'sqlite',
    table: ['Artist'],
  },
};

export const GDCUpdateTableCloneSelectPermission: StoryObj<PermissionsTabProps> =
  {
    args: {
      dataSourceName: 'sqlite',
      table: ['Artist'],
    },

    parameters: {
      msw: handlers(),
    },

    play: async ({ canvasElement }) => {
      const canvas = within(canvasElement);

      await canvas.findByTestId('permission-table-button-user-update');
      const tableUserUpdateButton = await canvas.getByTestId(
        'permission-table-button-user-update'
      );

      await userEvent.click(tableUserUpdateButton);
      const selectCheckbox = await canvas.findByTestId(
        'external-user-select-input-pre_update'
      );
      await userEvent.click(selectCheckbox);

      await canvas.findByTestId('external-check-json-editor');
      const rowPermissionBuilderContainer = await canvas.getByTestId(
        'external-check-json-editor'
      );

      await expect(
        rowPermissionBuilderContainer.getAttribute('data-state')
      ).toEqual('{"ArtistId":{"_eq":"X-Hasura-User-Id"}}');
    },
  };
