import type { ComponentMeta, ComponentStory } from '@storybook/react';
import type { ComponentPropsWithoutRef } from 'react';

import * as React from 'react';
import { expect } from '@storybook/jest';
import { act } from '@testing-library/react';
import { action } from '@storybook/addon-actions';
import { userEvent, within } from '@storybook/testing-library';

import type { FormValues } from './schema';
import { defaultValues } from './schema';
import { Form } from './Form';

export default {
  title: 'Features/OpenTelemetryConfig/Form',
  component: Form,
} as ComponentMeta<typeof Form>;

// --------------------------------------------------
// NOT TESTED
// --------------------------------------------------
// The following scenarios do not have interaction tests because...:
// - passing default values and expecting them to be rendered - not tested because it's a Form component feature
// - testing all the input fields error messages - not tested because it's a Form component feature that accepts a schema with error messages
// - testing the all open/close possibilities of the collapsible fields - not tested because it's a Collapsible component feature

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// DEFAULT STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Default: ComponentStory<typeof Form> = args => <Form {...args} />;

Default.storyName = '💠 Default';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const defaultStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  loading: false,
  onSubmit: action('onSubmit'),
};
Default.args = defaultStoryArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// SUBMITTING STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Submitting: ComponentStory<typeof Form> = args => (
  <Form {...args} />
);

Submitting.storyName = '💠 Loading';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const submittingStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  loading: true,
  onSubmit: action('onSubmit'),
};

Submitting.args = submittingStoryArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// HAPPY PATH TEST
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const HappyPath: ComponentStory<typeof Form> = args => (
  <Form {...args} />
);

HappyPath.storyName =
  '🧪 Testing - When filled up and submitted, the form must pass all the values';

HappyPath.parameters = { chromatic: { disableSnapshot: true } };

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const happyPathStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  loading: false,
  onSubmit: action('onSubmit'),
};
HappyPath.args = happyPathStoryArgs;

// --------------------------------------------------
// INTERACTION TEST
// --------------------------------------------------
HappyPath.play = async ({ args, canvasElement }) => {
  const canvas = within(canvasElement);

  // act avoids the "When testing, code that causes React state updates should be wrapped into act(...):" error
  await act(async () => {
    // STEP: Type the Endpoint
    await userEvent.type(
      await canvas.findByLabelText('Endpoint', { selector: 'input' }),
      'http://hasura.io'
    );

    // STEP: Type the Batch Size
    const batchSizeInputField = await canvas.findByLabelText('Batch Size', {
      selector: 'input',
    });
    await userEvent.clear(batchSizeInputField);
    await userEvent.type(batchSizeInputField, '100');

    // STEP: Open the collapsible headers section
    await userEvent.click(await canvas.findByText('Headers'));

    // STEP: Check the first empty header is already there when the collapsible section is opened!
    expect(
      canvas.getByRole('textbox', { name: 'headers[0].name' })
    ).toBeInTheDocument();
    expect(
      canvas.getByRole('textbox', { name: 'headers[0].value' })
    ).toBeInTheDocument();

    await userEvent.type(
      canvas.getByRole('textbox', { name: 'headers[0].name' }),
      'x-hasura-name'
    );
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'headers[0].value' }),
      'hasura_user'
    );

    // STEP: Add one more header
    const addNewRowButton = canvas.getByText('Add Headers');
    await userEvent.click(addNewRowButton);

    // STEP: Add an env-var header
    await userEvent.selectOptions(
      canvas.getByRole('combobox', { name: 'headers[1].type' }),
      'from_env'
    );
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'headers[1].name' }),
      'x-hasura-env'
    );
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'headers[1].value' }),
      'HASURA_USER'
    );

    // STEP: Open the collapsible attributes section
    await userEvent.click(await canvas.findByText('Attributes'));

    // STEP: Add an attribute
    // ATTENTION: a first empty attributes should be already there when the collapsible section is opened!
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'attributes[0].name' }),
      'foo'
    );
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'attributes[0].value' }),
      'bar'
    );

    // STEP: Click the Submit button
    const submitButton = await canvas.findByRole('button', { name: 'Update' });
    await userEvent.click(submitButton);
  });

  // @ts-expect-error arg.onSubmit is a Storybook action, hence a mock function, even if TS cannot
  // infer it from the story
  const onSubmitMock: jest.Mock = args.onSubmit;
  const receivedValues = onSubmitMock.mock.calls[0][0];

  // ATTENTION: The more idiomatic version of this assertion is:
  //  expect(args.onSubmit).toBeCalledWith(
  //    expect.objectContaining({ ...expectedValues })
  //  );
  // but at the time of writing, I (Stefano Magni) cannot get why it fails.
  // Hence the need to access mock.calls directly

  // STEP: Check the callback arguments
  expect(receivedValues).toMatchObject<FormValues>({
    endpoint: 'http://hasura.io',
    connectionType: 'http',
    dataType: 'traces',
    batchSize: 100,
    headers: [
      { name: 'x-hasura-name', type: 'from_value', value: 'hasura_user' },
      { name: 'x-hasura-env', type: 'from_env', value: 'HASURA_USER' },
    ],
    attributes: [{ name: 'foo', type: 'from_value', value: 'bar' }],
  });
};

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// PREVENT SUBMITTING TEST
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const PreventSubmitting: ComponentStory<typeof Form> = args => (
  <Form {...args} />
);

PreventSubmitting.storyName =
  '🧪 Testing - When loading, the form prevent submitting';

PreventSubmitting.parameters = { chromatic: { disableSnapshot: true } };

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const preventSubmittingStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  loading: true,
  onSubmit: action('onSubmit'),
};

PreventSubmitting.args = preventSubmittingStoryArgs;

// --------------------------------------------------
// INTERACTION TEST
// --------------------------------------------------
PreventSubmitting.play = async ({ args, canvasElement }) => {
  const canvas = within(canvasElement);
  const submitButton = await canvas.findByRole('button', {
    name: 'Updating...',
  });

  expect(submitButton).toHaveAttribute('disabled');

  // Try to click the submit button even if it's disabled
  userEvent.click(submitButton);

  // This assertion is reliable only if there is another test that checks that the onSubmit function
  // is called when it should be called (the happy path) otherwise it could result in false negatives
  // (for instance: if the form does not call the onSubmit function at all, even the submit button is
  // enabled)
  expect(args.onSubmit).not.toHaveBeenCalled();
};

// #endregion
