import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { StoryObj, Meta } from '@storybook/react';
import { RemoteSchema } from '../..';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import { within, userEvent, waitFor } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { handlers } from './mocks/handlers.mock';

export default {
  title: 'Features/Remote Schema/components/Create',
  component: RemoteSchema.Create,
  decorators: [ReactQueryDecorator(), ReduxDecorator({})],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof RemoteSchema.Create>;

export const Playground: StoryObj = {
  render: () => {
    const [showSuccessText, setShowSuccessText] = React.useState(false);
    const onSuccess = () => {
      setShowSuccessText(true);
    };
    return (
      <>
        <RemoteSchema.Create onSuccess={onSuccess} />;
        <p data-testid="@onSuccess">
          {showSuccessText ? 'Form saved successfully!' : null}
        </p>
      </>
    );
  },

  play: async ({ canvasElement }) => {
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

    await waitFor(
      async () => {
        await expect(await canvas.findByTestId('@onSuccess')).toHaveTextContent(
          'Form saved successfully!'
        );
      },
      { timeout: 5000 }
    );
  },
};
