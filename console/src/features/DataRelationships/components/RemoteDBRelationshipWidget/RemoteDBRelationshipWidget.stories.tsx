import React from 'react';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { within, userEvent, waitFor } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import {
  RemoteDBRelationshipWidget,
  RemoteDBRelationshipWidgetProps,
} from './RemoteDBRelationshipWidget';
import { handlers } from '../../../RemoteRelationships/RemoteSchemaRelationships/__mocks__/handlers.mock';

export default {
  title:
    'Features/Data Relationships/Remote DB Relationships/Remote DB Relationships Form',
  component: RemoteDBRelationshipWidget,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: Story<RemoteDBRelationshipWidgetProps> = args => (
  <RemoteDBRelationshipWidget {...args} />
);
Primary.args = {
  sourceTableInfo: {
    database: 'chinook',
    schema: 'public',
    table: 'Album',
  },
};

export const WithExistingRelationship: Story<RemoteDBRelationshipWidgetProps> =
  args => <RemoteDBRelationshipWidget {...args} />;
WithExistingRelationship.args = {
  ...Primary.args,
  existingRelationshipName: 'AlbumToResident',
};

let callbackResponsePrimaryWithTest = {};

export const PrimaryWithTest: Story<RemoteDBRelationshipWidgetProps> = args => (
  <RemoteDBRelationshipWidget
    {...args}
    onComplete={d => {
      callbackResponsePrimaryWithTest = d;
    }}
  />
);
PrimaryWithTest.args = { ...Primary.args };
PrimaryWithTest.parameters = {
  chromatic: { disableSnapshot: true },
};

PrimaryWithTest.play = async ({ canvasElement }) => {
  callbackResponsePrimaryWithTest = {};
  const canvas = within(canvasElement);

  const submitButton = await canvas.findByText('Save Relationship');

  userEvent.click(submitButton);

  const nameError = await canvas.findByText('Name is required!');
  const databaseError = await canvas.findByText(
    'Reference Database is required!'
  );
  const tableError = await canvas.findByText('Reference Table is required!');

  // expect error messages

  expect(nameError).toBeInTheDocument();
  expect(databaseError).toBeInTheDocument();
  expect(tableError).toBeInTheDocument();

  // update fields
  const nameInput = await canvas.findByLabelText('Name');
  userEvent.type(nameInput, 'test');

  const typeLabel = await canvas.findByLabelText('Type');
  const databaseLabel = await canvas.findByLabelText('Reference Database');
  const schemaLabel = await canvas.findByLabelText('Reference Schema');
  const tableLabel = await canvas.findByLabelText('Reference Table');

  userEvent.selectOptions(typeLabel, 'Array Relationship');
  userEvent.selectOptions(databaseLabel, 'default');
  userEvent.selectOptions(schemaLabel, 'public');
  userEvent.selectOptions(tableLabel, 'resident');
  userEvent.click(submitButton);
  await waitFor(() => {
    expect(canvas.queryByText('Saving relationship')).toBeInTheDocument();
  });

  await waitFor(() => {
    expect(JSON.stringify(callbackResponsePrimaryWithTest)).toContain(
      'Success'
    );
    expect(JSON.stringify(callbackResponsePrimaryWithTest)).toContain(
      'Relationship saved successfully'
    );
  });
};

let callbackResponseExistingRelationshipWithTest = {};

export const ExistingRelationshipWithTest: Story<RemoteDBRelationshipWidgetProps> =
  args => (
    <RemoteDBRelationshipWidget
      {...args}
      onComplete={d => {
        callbackResponseExistingRelationshipWithTest = d;
      }}
    />
  );
ExistingRelationshipWithTest.args = {
  ...Primary.args,
  existingRelationshipName: 'AlbumToResident',
};
ExistingRelationshipWithTest.parameters = {
  chromatic: { disableSnapshot: true },
};

ExistingRelationshipWithTest.play = async ({ canvasElement }) => {
  callbackResponseExistingRelationshipWithTest = {};
  const canvas = within(canvasElement);
  const submitButton = await canvas.findByText('Save Relationship');

  const relationshipType = await canvas.findByLabelText('Type');
  userEvent.selectOptions(relationshipType, 'Array Relationship');
  userEvent.click(submitButton);
  await waitFor(() => {
    expect(canvas.queryByText('Saving relationship')).toBeInTheDocument();
  });

  await waitFor(() => {
    expect(
      JSON.stringify(callbackResponseExistingRelationshipWithTest)
    ).toContain('Success');
    expect(
      JSON.stringify(callbackResponseExistingRelationshipWithTest)
    ).toContain('Relationship saved successfully');
  });
};
