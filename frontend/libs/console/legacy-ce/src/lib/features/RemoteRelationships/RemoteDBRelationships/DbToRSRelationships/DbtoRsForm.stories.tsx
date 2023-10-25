import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { dangerouslyDelay } from '../../../../storybook/utils/dangerouslyDelay';
import { handlers } from '../../../RemoteRelationships/RemoteSchemaRelationships/__mocks__/handlers.mock';
import { DbToRsForm, DbToRsFormProps } from './DbToRsForm';

export default {
  title: 'Features/Data Relationships/DB to RS Relationship Form',
  component: DbToRsForm,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: StoryObj<DbToRsFormProps> = {
  render: args => (
    <div>
      <h1 className="text-2xl bold">DbToRsForm</h1>
      <br />
      <h1 className="text-xl">What does this component do?</h1>
      <br />
      <p>
        DbToRsForm component can be used for both creating & editing a DB to RS
        Relationship
      </p>
      <br />
      <p>
        To make the component function in create mode, the only prop that needs
        to be passed is the sourceTableInfo. the other requirements like the
        schema details, list of remote schemas etc. would be fetched internally
        with the help of react-query. <br />
        <br />
        This component can also call the API and update the metadata internally
        without any external configuration, however if we need to listen to the
        onComplete event, there is an optional callback that can be used for
        this
      </p>
      <br />

      <br />
      <br />
      <DbToRsForm {...args} />
    </div>
  ),

  args: {
    sourceTableInfo: {
      database: 'chinook',
      schema: 'public',
      table: 'Artist',
    },
  },
};

export const PlaygroundWithPrimaryTest: StoryObj<DbToRsFormProps> = {
  render: args => (
    <DbToRsForm
      {...args}
      onComplete={d => {
        // eslint-disable-next-line no-alert
        alert(JSON.stringify(d, null, 2));
      }}
    />
  ),

  args: Primary.args,

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await canvas.findByText('Name');

    const submitButton = await canvas.findByText('Save Relationship');

    userEvent.click(submitButton);

    const nameError = await canvas.findByText('Name is required!');

    // expect error messages
    expect(nameError).toBeInTheDocument();

    // update fields
    const nameInput = await canvas.findByLabelText('Name');
    userEvent.type(nameInput, 'test');

    await waitFor(() => {
      expect(canvas.getByTestId('source_database')).toBeInTheDocument();
    });

    await userEvent.selectOptions(
      canvas.getByTestId('source_database'),
      'chinook'
    );
    await userEvent.selectOptions(
      canvas.getByTestId('source_schema'),
      'public'
    );
    await userEvent.selectOptions(canvas.getByTestId('source_table'), 'Artist');

    userEvent.click(submitButton);

    const referenceSchema = await canvas.findByText('Select a remote schema');
    userEvent.click(referenceSchema);

    await userEvent.click(await canvas.findByText('remoteSchema2'), undefined, {
      skipHover: true,
    });
    userEvent.click(submitButton);
  },
};

export const EditMode: StoryObj<DbToRsFormProps> = {
  render: args => (
    <div>
      <h1 className="text-2xl bold">DbToRsForm</h1>
      <br />
      <h1 className="text-xl">What does this component do?</h1>
      <br />
      <p>
        DbToRsForm component can be used for both creating & editing a DB to RS
        Relationship{' '}
      </p>
      <br />
      <p>
        In order to render the component in edit mode, you could pass the
        selectedRelationship property and that will be used to render the pre
        opened explorer view. <br />
        considering this relationship object would be provided by the table, and
        table has already processed the relationship object into the new format
        selectedRelationship would be accepting only the new format
      </p>
      <br />
      <br />
      <br />
      <br />
      <DbToRsForm
        {...args}
        onComplete={d => {
          // eslint-disable-next-line no-alert
          alert(JSON.stringify(d, null, 2));
        }}
      />
    </div>
  ),

  args: {
    sourceTableInfo: {
      database: 'chinook',
      schema: 'public',
      table: 'Artist',
    },
    selectedRelationship: {
      definition: {
        to_remote_schema: {
          remote_schema: 'remoteSchema2',
          lhs_fields: ['Title'],
          remote_field: {
            continents: { arguments: { filter: { code: { eq: '$Title' } } } },
          },
        },
      },
      name: 'new_payload',
    },
  },
};

let callbackResponse = {};

export const PlaygroundWithEditModeTest: StoryObj<DbToRsFormProps> = {
  render: args => (
    <DbToRsForm
      {...args}
      onComplete={d => {
        callbackResponse = d;
      }}
    />
  ),

  args: EditMode.args,

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const submitButton = await canvas.findByText('Save Relationship');

    await dangerouslyDelay(1000); // time to settle down the mock response (introspection)

    userEvent.click(submitButton);

    await dangerouslyDelay(1000); // time to settle down the mock response (create relationship)

    expect(JSON.stringify(callbackResponse)).toContain('Success');
    expect(JSON.stringify(callbackResponse)).toContain(
      'Relationship saved successfully'
    );
  },
};
