import React from 'react';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { handlers } from './mocks/handlers.mock';
import RemoteSchemaForm from './Form';
import { createRemoteSchemaFormValues } from './utils';

export default {
  title: 'Features/Remote Schema/components/Create',
  component: RemoteSchemaForm,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof RemoteSchemaForm>;

export const Playground: StoryObj = {
  render: () => {
    const [showSuccessText, setShowSuccessText] = React.useState(false);
    const onSuccess = async () => {
      setShowSuccessText(true);
    };
    return (
      <>
        <RemoteSchemaForm
          onSubmit={onSuccess}
          defaultValues={createRemoteSchemaFormValues()}
          saving={false}
        />
        ;
        <p data-testid="@onSuccess">
          {showSuccessText ? 'Form saved successfully!' : null}
        </p>
      </>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(await canvas.findByTestId('submit'));

    // expect error messages
    await expect(
      await canvas.findByText('Remote Schema name is a required field!'),
    ).toBeInTheDocument();

    // Fill up the fields
    // const nameInput = await canvas.findByLabelText('Name');
    await userEvent.type(await canvas.findByTestId('name'), 'test');
    await userEvent.type(
      await canvas.findByTestId('url'),
      'http://example.com',
    );
    await userEvent.type(await canvas.findByTestId('timeout_seconds'), '180');
    await userEvent.click(await canvas.findByTestId('forward_client_headers'));

    await userEvent.click(await canvas.findByTestId('open_customization'));

    await userEvent.type(
      await canvas.findByTestId('customization.root_fields_namespace'),
      'root_field_namespace_example',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.type_prefix'),
      'type_prefix_example_',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.type_suffix'),
      '_type_suffix_example',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.query_root.parent_type'),
      'query_root_parent_type_example_',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.query_root.prefix'),
      'query_root_prefix_example_',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.query_root.suffix'),
      '_query_root_suffix_example',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.mutation_root.parent_type'),
      'mutation_root_parent_type_example_',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.mutation_root.prefix'),
      'mutation_root_prefix_example_',
    );
    await userEvent.type(
      await canvas.findByTestId('customization.mutation_root.suffix'),
      '_mutation_root_suffix_example',
    );

    await userEvent.click(await canvas.findByTestId('submit'));

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('@onSuccess')).toHaveTextContent(
          'Form saved successfully!',
        );
      },
      { timeout: 5000 },
    );
  },
};
