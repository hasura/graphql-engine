import type { Meta, StoryObj } from '@storybook/react';
import type { ComponentPropsWithoutRef } from 'react';

import { action } from '@storybook/addon-actions';
import { expect } from '@storybook/jest';
import { userEvent, within } from '@storybook/testing-library';

import { Form } from './Form';
import { defaultValues } from './schema';

export default {
  title: 'Features/OpenTelemetry/Form',
  component: Form,
} as Meta<typeof Form>;

const happyPathStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  skeletonMode: false,
  firstTimeSetup: true,
  onSubmit: action('onSubmit'),
};

const connectButtonStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  skeletonMode: false,
  firstTimeSetup: false,
  onSubmit: action('onSubmit'),
};

export const ConnectButton: StoryObj<typeof Form> = {
  name: `🧪 Testing - When it's not the first-time setup, the button should have the text "Update"`,
  parameters: { chromatic: { disableSnapshot: true } },
  args: connectButtonStoryArgs,

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const updateButton = await canvas.getByRole('button', { name: 'Update' });
    expect(updateButton).toBeVisible();
  },
};

const defaultStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  skeletonMode: false,
  firstTimeSetup: true,
  onSubmit: action('onSubmit'),
};

export const Default: StoryObj<typeof Form> = {
  name: '💠 Default',
  args: defaultStoryArgs,
};

const skeletonStoryArgs: ComponentPropsWithoutRef<typeof Form> = {
  defaultValues,
  skeletonMode: true,
  firstTimeSetup: true,
  onSubmit: action('onSubmit'),
};
export const Skeleton: StoryObj<typeof Form> = {
  name: '💠 Skeleton',
  args: skeletonStoryArgs,
};

export const HappyPath: StoryObj<typeof Form> = {
  name: '🧪 Testing - When filled up and submitted, the form must pass all the values',
  parameters: { chromatic: { disableSnapshot: true } },
  args: happyPathStoryArgs,

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    // act avoids the "When testing, code that causes React state updates should be wrapped into act(...):" error

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

    const addNewHeaderButton = canvas.getByText('Add Headers');
    // STEP: Add one more header
    await userEvent.click(addNewHeaderButton);

    await userEvent.type(
      canvas.getByRole('textbox', { name: 'headers[0].name' }),
      'x-hasura-name'
    );
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'headers[0].value' }),
      'hasura_user'
    );

    // STEP: Add one more header
    await userEvent.click(addNewHeaderButton);

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

    const addNewAttributeButton = canvas.getByText('Add Attributes');
    // STEP: Add one more attribute
    await userEvent.click(addNewAttributeButton);

    // STEP: Add an attribute
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'attributes[0].name' }),
      'foo'
    );
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'attributes[0].value' }),
      'bar'
    );

    // STEP: Click the Submit button
    await userEvent.click(await canvas.findByText('Connect'));

    // // @ts-expect-error arg.onSubmit is a Storybook action, hence a mock function, even if TS cannot
    // // infer it from the story
    // const onSubmitMock: jest.Mock = args.onSubmit;
    // const receivedValues = onSubmitMock.mock.calls[0][0];

    // // ATTENTION: The more idiomatic version of this assertion is:
    // //  expect(args.onSubmit).toBeCalledWith(
    // //    expect.objectContaining({ ...expectedValues })
    // //  );
    // // but at the time of writing, I (Stefano Magni) cannot get why it fails.
    // // Hence the need to access mock.calls directly

    // // STEP: Check the callback arguments
    // expect(receivedValues).toMatchObject<FormValues>({
    //   enabled: true,
    //   endpoint: 'http://hasura.io',
    //   connectionType: 'http/protobuf',
    //   dataType: ['traces'],
    //   batchSize: 100,
    //   headers: [
    //     { name: 'x-hasura-name', type: 'from_value', value: 'hasura_user' },
    //     { name: 'x-hasura-env', type: 'from_env', value: 'HASURA_USER' },
    //   ],
    //   attributes: [{ name: 'foo', value: 'bar' }],
    // });
  },
};
