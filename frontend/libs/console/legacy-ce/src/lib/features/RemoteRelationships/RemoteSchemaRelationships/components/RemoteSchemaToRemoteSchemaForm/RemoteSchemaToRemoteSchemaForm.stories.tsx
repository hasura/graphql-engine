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

    await canvas.findByText('Relationship Name');

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
    const nameInput = await canvas.findByLabelText('Relationship Name');
    userEvent.type(nameInput, 'test');

    const sourceType = await canvas.findByText('Select a type');
    userEvent.click(sourceType);
    await userEvent.click(await canvas.findByText('Continent'), undefined, {
      skipHover: true,
    });

    const referenceSchema = await canvas.findByText('Select a remote schema');
    userEvent.click(referenceSchema);
    await userEvent.click(await canvas.findByText('remoteSchema2'), undefined, {
      skipHover: true,
    });

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
