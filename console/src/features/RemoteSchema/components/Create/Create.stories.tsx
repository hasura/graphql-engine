import React, { useReducer } from 'react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta, Story } from '@storybook/react';
import { RemoteSchema } from '@/features/RemoteSchema';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'RemoteSchema/components/Create',
  component: RemoteSchema.Create,
  decorators: [ReactQueryDecorator(), ReduxDecorator({})],
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof RemoteSchema.Create>;

export const Playground: Story = () => {
  const [formSuccess, toggle] = useReducer(s => !s, false);
  return (
    <>
      <RemoteSchema.Create onSuccess={() => toggle()} />
      <div>{formSuccess ? 'Form saved succesfully!' : null}</div>
    </>
  );
};

Playground.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  userEvent.click(await canvas.findByTestId('submit'));

  // expect error messages
  expect(
    await canvas.findByText('Remote Schema name is a required field!')
  ).toBeInTheDocument();

  // Fill up the fields
  // const nameInput = await canvas.findByLabelText('Name');
  userEvent.type(await canvas.findByTestId('name'), 'test');
  userEvent.type(await canvas.findByTestId('url'), 'http://example.com');
  userEvent.type(await canvas.findByTestId('timeout_seconds'), '180');
  userEvent.click(await canvas.findByTestId('forward_client_headers'));

  userEvent.click(await canvas.findByTestId('open_customization'));

  userEvent.type(
    await canvas.findByTestId('customization.root_fields_namespace'),
    'root_field_namespace_example'
  );
  userEvent.type(
    await canvas.findByTestId('customization.type_prefix'),
    'type_prefix_example_'
  );
  userEvent.type(
    await canvas.findByTestId('customization.type_suffix'),
    '_type_suffix_example'
  );
  userEvent.type(
    await canvas.findByTestId('customization.query_root.parent_type'),
    'query_root_parent_type_example_'
  );
  userEvent.type(
    await canvas.findByTestId('customization.query_root.prefix'),
    'query_root_prefix_example_'
  );
  userEvent.type(
    await canvas.findByTestId('customization.query_root.suffix'),
    '_query_root_suffix_example'
  );
  userEvent.type(
    await canvas.findByTestId('customization.mutation_root.parent_type'),
    'mutation_root_parent_type_example_'
  );
  userEvent.type(
    await canvas.findByTestId('customization.mutation_root.prefix'),
    'mutation_root_prefix_example_'
  );
  userEvent.type(
    await canvas.findByTestId('customization.mutation_root.suffix'),
    '_mutation_root_suffix_example'
  );

  userEvent.click(await canvas.findByTestId('submit'));

  expect(
    await canvas.findByText('Form saved succesfully!')
  ).toBeInTheDocument();
};
