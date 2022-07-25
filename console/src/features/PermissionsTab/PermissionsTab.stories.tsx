import React from 'react';
import { Story, Meta } from '@storybook/react';
import { within, waitFor, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

import { ReactQueryDecorator } from '@/storybook/decorators/react-query';

import { PermissionsTab, PermissionsTabProps } from './PermissionsTab';
import { handlers } from '../PermissionsForm/mocks/handlers.mock';

export default {
  title: 'Features/Permissions Tab/Permissions Tab',
  component: PermissionsTab,
  decorators: [ReactQueryDecorator()],
} as Meta;

export const Primary: Story<PermissionsTabProps> = args => (
  <PermissionsTab {...args} />
);
Primary.args = {
  params: {
    tab: '',
    schema: 'public',
    table: 'users',
  },
};

Primary.parameters = {
  msw: handlers(),
};

export const BasicInteraction: Story<PermissionsTabProps> = args => (
  <PermissionsTab {...args} />
);

BasicInteraction.args = Primary.args;
BasicInteraction.parameters = Primary.parameters;

BasicInteraction.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // open user insert section
  const userInsertButton = await canvas.findByLabelText('user-insert');
  userEvent.click(userInsertButton);

  // change input of row section
  const input: HTMLInputElement = await waitFor(() =>
    canvas.findByDisplayValue('1')
  );
  userEvent.clear(input);
  userEvent.type(input, '2');

  // change selections of selected columns
  const nameCheckbox: HTMLInputElement = await canvas.findByLabelText('name');
  userEvent.click(nameCheckbox);

  // toggle all options on
  const toggleAllBtn = await canvas.getByRole('button', { name: 'Toggle All' });
  userEvent.click(toggleAllBtn);

  // open backend only section and select
  const backendOnly = await canvas.findByText('Backend only');
  userEvent.click(backendOnly);
  const backendOnlyCheckbox: HTMLInputElement = await canvas.findByLabelText(
    'Allow from backends only'
  );
  userEvent.click(backendOnlyCheckbox);

  // check interactions were successful
  expect(input.value).toEqual('2');
  expect(backendOnlyCheckbox.checked).toBe(true);
  expect(nameCheckbox.checked).toBe(true);
};

export const MultipleInteractions: Story<PermissionsTabProps> = args => (
  <PermissionsTab {...args} />
);

MultipleInteractions.args = Primary.args;
MultipleInteractions.parameters = Primary.parameters;

MultipleInteractions.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  // click on new role item
  const newRoleInsertButton = await canvas.findByLabelText('-insert');
  userEvent.click(newRoleInsertButton);
  await new Promise(r => setTimeout(r, 200));

  // check role input becomes focused
  const newRoleInput: HTMLInputElement = await canvas.findByPlaceholderText(
    'Create new role...'
  );
  expect(document.activeElement).toEqual(newRoleInput);

  userEvent.type(newRoleInput, 'new_role');
  userEvent.click(newRoleInsertButton);

  await new Promise(r => setTimeout(r, 200));

  // expect form to now be open
  const closeBtn = canvas.getByRole('button', { name: 'Close' });
  expect(closeBtn).toBeTruthy();

  const noChecksLabel = await canvas.findByLabelText('Without any checks');
  userEvent.click(noChecksLabel);

  // click on bulk update
  const userCheckBox = await canvas.findByLabelText('user');
  userEvent.click(userCheckBox);

  // expect new role name to be cleared
  expect(newRoleInput.value).toEqual('');

  // expect bulk update to be open
  const removePermissionsBtn = canvas.getByRole('button', {
    name: 'Remove All Permissions',
  });
  expect(removePermissionsBtn).toBeTruthy();

  // expect main for to be closed
  expect(canvas.queryByText('Close')).toBeFalsy();
};
