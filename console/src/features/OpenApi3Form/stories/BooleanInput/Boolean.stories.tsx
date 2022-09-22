import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { screen } from '@testing-library/dom';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { RenderOpenApi3Form } from '../common/RenderOpenApi3Form';

export default {
  title: 'Components/OpenApi3Form ⚛️ /Boolean',
  parameters: {
    docs: {
      description: {
        component: `This component demonstrates how to use a boolean input via the OpenApi3Form component`,
      },
      source: { type: 'code' },
    },
  },
  component: RenderOpenApi3Form,
  decorators: [
    ReactQueryDecorator(),
    Story => <div className="p-4 w-full">{Story()}</div>,
  ],
} as ComponentMeta<typeof RenderOpenApi3Form>;

export const booleanInput: ComponentStory<typeof RenderOpenApi3Form> = () => {
  return (
    <RenderOpenApi3Form
      name="BooleanInput"
      getSchema={() => [
        {
          title: 'Boolean Input (nullable set to false)',
          type: 'boolean',
          nullable: false,
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const booleanInputWithExistingValues: ComponentStory<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="booleanInput"
      getSchema={() => [
        {
          title: 'Boolean Input (with existing value)',
          type: 'boolean',
        },
        {},
      ]}
      defaultValues={{
        booleanInput: true,
      }}
    />
  );
};

export const Test: ComponentStory<typeof RenderOpenApi3Form> = () => (
  <RenderOpenApi3Form
    name="BooleanInput"
    getSchema={() => [
      {
        title: 'Boolean Input (nullable set to false)',
        type: 'boolean',
        nullable: false,
      },
      {},
    ]}
    defaultValues={{}}
    rawOutput
  />
);

Test.storyName = '🧪 Testing - toggle interaction';

Test.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await waitFor(async () => {
    await userEvent.click(canvas.getByTestId('BooleanInput'));
  });

  await waitFor(async () => {
    await userEvent.click(canvas.getByTestId('submit-form-btn'));
  });

  await waitFor(async () => {
    await expect(screen.getByTestId('output').textContent).toBe(
      '{"BooleanInput":true}'
    );
  });
};
