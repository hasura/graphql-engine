import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import {
  LocalRelationshipWidget,
  LocalRelationshipWidgetProps,
} from './LocalRelationshipWidget';
import { handlers } from '../../../RemoteRelationships/RemoteSchemaRelationships/__mocks__/handlers.mock';

export default {
  title:
    'Data Relationships/Local DB Relationships/Local DB Relationships Form',
  component: LocalRelationshipWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: Story<LocalRelationshipWidgetProps> = args => (
  <LocalRelationshipWidget {...args} />
);
Primary.args = {
  sourceTableInfo: {
    database: 'chinook',
    schema: 'public',
    table: 'Album',
  },
};

export const WithExistingObjectRelationship: Story<LocalRelationshipWidgetProps> = args => (
  <LocalRelationshipWidget {...args} />
);
WithExistingObjectRelationship.args = {
  ...Primary.args,
  existingRelationshipName: 'relt1obj',
};

export const WithExistingArrayRelationship: Story<LocalRelationshipWidgetProps> = args => (
  <LocalRelationshipWidget {...args} />
);
WithExistingArrayRelationship.args = {
  ...Primary.args,
  existingRelationshipName: 'relt1array',
};

export const PrimaryWithTest: Story<LocalRelationshipWidgetProps> = args => (
  <LocalRelationshipWidget {...args} />
);
PrimaryWithTest.args = { ...Primary.args };

PrimaryWithTest.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  const submitButton = await canvas.findByText('Save Relationship');

  userEvent.click(submitButton);

  const nameError = await canvas.findByText('Name is required!');
  const tableError = await canvas.findByText('Reference Table is required!');

  // expect error messages

  expect(nameError).toBeInTheDocument();
  expect(tableError).toBeInTheDocument();

  // update fields
  const nameInput = await canvas.findByLabelText('Name');
  userEvent.type(nameInput, 'test');

  const typeLabel = await canvas.findByLabelText('Type');
  const schemaLabel = await canvas.findByLabelText('Reference Schema');
  const tableLabel = await canvas.findByLabelText('Reference Table');

  userEvent.selectOptions(typeLabel, 'Array Relationship');
  userEvent.selectOptions(schemaLabel, 'user');
  userEvent.selectOptions(tableLabel, 'userAddress');
  userEvent.click(submitButton);
};
