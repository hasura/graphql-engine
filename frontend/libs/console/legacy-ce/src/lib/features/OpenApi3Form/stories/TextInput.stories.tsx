import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import React from 'react';
import { screen } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { RenderOpenApi3Form } from './common/RenderOpenApi3Form';

export default {
  title: 'Components/OpenApi3Form ⚛️/TextInput',
  component: RenderOpenApi3Form,
  parameters: {
    docs: {
      description: {
        component: `This component demonstrates how to use a string input via the OpenApi3Form component`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    ReactQueryDecorator(),
    Story => <div className="p-4 w-full">{Story()}</div>,
  ],
} as Meta<typeof RenderOpenApi3Form>;

export const TextInput: StoryFn<typeof RenderOpenApi3Form> = () => {
  return (
    <RenderOpenApi3Form
      name="textInput"
      getSchema={() => [
        {
          title: 'Text Input (nullable set to false)',
          type: 'string',
          nullable: false,
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const TextInputWithNullableTrue: StoryFn<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="textInput"
      getSchema={() => [
        {
          title: 'Text Input (nullable set to false)',
          type: 'string',
          nullable: true,
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const TextInputWithNullablePropertyMissing: StoryFn<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="textInput"
      getSchema={() => [
        {
          title: 'Text Input (no nullable key present)',
          type: 'string',
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const TextInputWithExistingValues: StoryFn<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="textInput"
      getSchema={() => [
        {
          title: 'Text Input (with existing value)',
          type: 'string',
        },
        {},
      ]}
      defaultValues={{
        textInput: 'Lorem ipsum dolor sit amet',
      }}
    />
  );
};

export const Test: StoryObj<typeof RenderOpenApi3Form> = {
  render: () => (
    <RenderOpenApi3Form
      name="textInput"
      getSchema={() => [
        {
          title: 'Text Input (nullable set to false)',
          type: 'string',
          nullable: false,
        },
        {},
      ]}
      defaultValues={{}}
      rawOutput
    />
  ),

  name: '🧪 Testing - input interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(async () => {
      await userEvent.type(
        canvas.getByTestId('textInput'),
        'some random value'
      );
    });

    await waitFor(async () => {
      await userEvent.click(canvas.getByTestId('submit-form-btn'));
    });

    await waitFor(async () => {
      await expect(screen.getByTestId('output').textContent).toBe(
        '{"textInput":"some random value"}'
      );
    });
  },
};
