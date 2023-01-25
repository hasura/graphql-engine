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
// - testing that the submit button is disabled when the form is submitting - not tested because it's a Form component feature

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
  skeletonMode: false,
  onSubmit: action('onSubmit'),
};
Default.args = defaultStoryArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// SKELETON PATH TEST
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Skeleton: ComponentStory<typeof Form> = args => <Form {...args} />;

Skeleton.storyName = '💠 Skeleton';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const skeletonStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  skeletonMode: true,
  onSubmit: action('onSubmit'),
};

Skeleton.args = skeletonStoryArgs;

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
  skeletonMode: false,
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
    // STEP: Enable OpenTelemetry
    await userEvent.click(await canvas.findByLabelText('Status'));

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
    enabled: true,
    endpoint: 'http://hasura.io',
    connectionType: 'http/protobuf',
    dataType: ['traces'],
    batchSize: 100,
    headers: [
      { name: 'x-hasura-name', type: 'from_value', value: 'hasura_user' },
      { name: 'x-hasura-env', type: 'from_env', value: 'HASURA_USER' },
    ],
    attributes: [{ name: 'foo', type: 'from_value', value: 'bar' }],
  });
};

// #endregion
