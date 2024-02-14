import React from 'react';
import { ComponentMeta } from '@storybook/react';
import { InputValidation, inputValidationSchema } from './InputValidation';
import { SimpleForm } from '../../../../../new-components/Form';
import { userEvent, within } from '@storybook/testing-library';

export default {
  title: 'Features/Table Permissions/Input Validation',
  component: InputValidation,
} as ComponentMeta<typeof InputValidation>;

export const Base = () => (
  <SimpleForm schema={inputValidationSchema} onSubmit={() => {}}>
    <InputValidation />
  </SimpleForm>
);

Base.play = async ({ canvasElement }: any) => {
  const canvas = within(canvasElement);

  // --------------------------------------------------
  // Step 1: Open the collapsible form
  await userEvent.click(canvas.getByText('Input Validation'));

  // --------------------------------------------------
  // Step 2: Enable the input validation
  await userEvent.click(canvas.getByTestId('enableValidation'));

  // --------------------------------------------------
  // Step 3: Fill up webhook url
  await userEvent.type(
    canvas.getByRole('textbox', {
      name: 'Webhook URL',
    }),
    'http://webhook.com'
  );

  // --------------------------------------------------
  // Step 4: Fill up timeout
  await userEvent.type(canvas.getByPlaceholderText('10 (default)'), '60');

  // --------------------------------------------------
  // Step 5: enable forward client header
  await userEvent.click(canvas.getByText('Forward client headers to webhook'));

  // --------------------------------------------------
  // Step 6: add headers
  await userEvent.click(canvas.getByText('Add Additional Headers'));
  await userEvent.type(canvas.getByPlaceholderText('Key'), 'x-hasura-name');
  await userEvent.type(
    canvas.getByPlaceholderText('Value or {{Environment_Variable}}'),
    'hasura_user'
  );
};
