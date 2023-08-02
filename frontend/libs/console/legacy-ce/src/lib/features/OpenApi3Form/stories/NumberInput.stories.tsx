import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import React from 'react';
import { screen } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { RenderOpenApi3Form } from './common/RenderOpenApi3Form';

export default {
  title: 'Components/OpenApi3Form ⚛️/NumberInput',
  parameters: {
    docs: {
      description: {
        component: `This component demonstrates how to use a number input via the OpenApi3Form component`,
      },
      source: { type: 'code' },
    },
  },
  component: RenderOpenApi3Form,
  decorators: [
    ReactQueryDecorator(),
    Story => <div className="p-4 w-full">{Story()}</div>,
  ],
} as Meta<typeof RenderOpenApi3Form>;

export const NumberInput: StoryFn<typeof RenderOpenApi3Form> = () => {
  return (
    <RenderOpenApi3Form
      name="numberInput"
      getSchema={() => [
        {
          title: 'Number Input (nullable set to false)',
          type: 'number',
          nullable: false,
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const NumberInputWithNullableTrue: StoryFn<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="numberInput"
      getSchema={() => [
        {
          title: 'Number Input (nullable set to false)',
          type: 'number',
          nullable: true,
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const NumberInputWithNullablePropertyMissing: StoryFn<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="numberInput"
      getSchema={() => [
        {
          title: 'Number Input (no nullable key present)',
          type: 'number',
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const NumberInputWithExistingValues: StoryFn<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="numberInput"
      getSchema={() => [
        {
          title: 'Number Input (with existing value)',
          type: 'number',
        },
        {},
      ]}
      defaultValues={{
        numberInput: 1234560,
      }}
    />
  );
};

export const Test: StoryObj<typeof RenderOpenApi3Form> = {
  render: () => (
    <RenderOpenApi3Form
      name="numberInput"
      getSchema={() => [
        {
          title: 'Number Input (nullable set to false)',
          type: 'number',
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
      await userEvent.type(canvas.getByTestId('numberInput'), '1234');
    });

    await waitFor(async () => {
      await userEvent.click(canvas.getByTestId('submit-form-btn'));
    });

    await waitFor(async () => {
      await expect(screen.getByTestId('output').textContent).toBe(
        '{"numberInput":1234}'
      );
    });
  },
};
