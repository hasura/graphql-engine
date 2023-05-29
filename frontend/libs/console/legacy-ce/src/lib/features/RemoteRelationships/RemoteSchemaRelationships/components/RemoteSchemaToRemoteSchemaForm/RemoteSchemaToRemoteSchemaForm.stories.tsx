import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

import {
  RemoteSchemaToRemoteSchemaForm,
  RemoteSchemaToRemoteSchemaFormProps,
} from './RemoteSchemaToRemoteSchemaForm';

import { handlers } from '../../__mocks__';

export default {
  title:
    'Features/Remote Relationships/Components/Remote Schema To Remote Schema Form',
  component: RemoteSchemaToRemoteSchemaForm,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: StoryObj<RemoteSchemaToRemoteSchemaFormProps> = {
  args: {
    sourceRemoteSchema: 'countries',
    closeHandler: () => {},
  },
};

export const PrimaryWithTest: StoryObj<RemoteSchemaToRemoteSchemaFormProps> = {
  args: Primary.args,

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await canvas.findByText('Name');

    const submitButton = (await canvas.findAllByText('Add Relationship'))[1];

    userEvent.click(submitButton);

    const nameError = await canvas.findByText('Name is required');
    const selectError = await canvas.findByText(
      'Related remote schema is required'
    );

    // expect error messages
    expect(nameError).toBeInTheDocument();
    expect(selectError).toBeInTheDocument();

    // update fields
    const nameInput = await canvas.findByLabelText('Name');
    userEvent.type(nameInput, 'test');

    const sourceType = await canvas.findByLabelText('Source Type');

    userEvent.selectOptions(sourceType, 'Continent');
    userEvent.click(submitButton);

    const referenceSchema = await canvas.findByLabelText(
      'Reference Remote Schema'
    );

    userEvent.selectOptions(referenceSchema, 'remoteSchema2');
    userEvent.click(submitButton);
  },
};

export const WithExistingRelationship: StoryObj<RemoteSchemaToRemoteSchemaFormProps> =
  {
    args: {
      ...Primary.args,
      sourceRemoteSchema: 'with_default_values',
      typeName: 'Country',
      existingRelationshipName: 'an_example_rs_to_rs_relationship',
    },
  };
