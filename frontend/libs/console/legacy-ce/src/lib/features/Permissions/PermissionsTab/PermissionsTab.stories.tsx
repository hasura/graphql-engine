import { Story, Meta } from '@storybook/react';

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

export const GDC: Story<PermissionsTabProps> = args => (
  <PermissionsTab {...args} />
);
GDC.args = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
};

GDC.parameters = {
  msw: handlers(),
};

export const GDCNoMocks: Story<PermissionsTabProps> = args => (
  <PermissionsTab {...args} />
);
GDCNoMocks.args = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
};

export const GDCUpdateTableCloneSelectPermission: Story<
  PermissionsTabProps
> = args => <PermissionsTab {...args} />;

GDCUpdateTableCloneSelectPermission.args = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
};

GDCUpdateTableCloneSelectPermission.parameters = {
  msw: handlers(),
};

GDCUpdateTableCloneSelectPermission.play = async ({ canvasElement }) => {
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
};

// export const GDCUpdateTableCreatePermissions: Story<
//   PermissionsTabProps
// > = args => <PermissionsTab {...args} />;

// GDCUpdateTableCreatePermissions.args = {
//   dataSourceName: 'sqlite',
//   table: ['Artist'],
// };

// GDCUpdateTableCreatePermissions.parameters = {
//   msw: handlers(),
// };

// GDCUpdateTableCreatePermissions.play = async ({ canvasElement }) => {
//   const canvas = within(canvasElement);

//   await waitFor(
//     async () => await canvas.getByTestId('permission-table-button-user-update')
//   );
//   const tableUserUpdateButton = await canvas.getByTestId(
//     'permission-table-button-user-update'
//   );

//   await userEvent.click(tableUserUpdateButton);
//   await waitFor(async () => await canvas.getByTestId(`permissions-form`));

//   const preSelectCheckbox = await canvas.getByTestId(
//     `custom-user-update-input-pre`
//   );
//   await userEvent.click(preSelectCheckbox);

//   await waitFor(async () => await canvas.getByTestId('-operator'));
//   await userEvent.selectOptions(canvas.getByTestId('-operator'), '_and');

//   await waitFor(async () => await canvas.getByText(`ArtistId`), {
//     timeout: 15000,
//   });

//   await userEvent.selectOptions(
//     canvas.getByTestId('_and.1-operator'),
//     'ArtistId'
//   );

//   await userEvent.type(
//     canvas.getByTestId('_and.1.ArtistId._eq-value-input'),
//     '1337',
//     { delay: 300 }
//   );

//Fails to run the consecutively. Putting on hold for future ticket.
// const preSelectCheckboxPost = await canvas.getByTestId(
//   `custom-user-update-input-post`
// );
// await userEvent.click(preSelectCheckboxPost);

// await waitFor(async() => await canvas.getByTestId('-operator'));
// await userEvent.selectOptions(canvas.getByTestId('-operator'), '_and');

// await waitFor(async() => await canvas.getByText(
//   `ArtistId`
// ), {timeout: 15000});

// await userEvent.selectOptions(
//   canvas.getByTestId('_and.1-operator'),
//   'ArtistId'
// );

// await userEvent.type(
//   canvas.getByTestId('_and.1.ArtistId._eq-value-input'),
//   '1337',
//   { delay: 300 }
// );
// };
