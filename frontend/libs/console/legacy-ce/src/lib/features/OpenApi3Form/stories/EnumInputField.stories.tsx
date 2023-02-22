import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import React from 'react';
import { screen } from '@testing-library/dom';
import { expect } from '@storybook/jest';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { RenderOpenApi3Form } from './common/RenderOpenApi3Form';

export default {
  title: 'Components/OpenApi3Form ⚛️/EnumInput',
  component: RenderOpenApi3Form,
  parameters: {
    docs: {
      description: {
        component: `This component demonstrates how to use a Enum input via the OpenApi3Form component`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    ReactQueryDecorator(),
    Story => <div className="p-4 w-full">{Story()}</div>,
  ],
} as ComponentMeta<typeof RenderOpenApi3Form>;

export const EnumInput: ComponentStory<typeof RenderOpenApi3Form> = () => {
  return (
    <RenderOpenApi3Form
      name="enumInput"
      getSchema={() => [
        {
          title: 'Enum Input',
          type: 'string',
          enum: ['option1', 'option2'],
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const EnumInputWithNullableAsOption: ComponentStory<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="enumInput"
      getSchema={() => [
        {
          title: 'Enum Input',
          type: 'string',
          enum: ['option1', 'option2', null],
        },
        {},
      ]}
      defaultValues={{}}
    />
  );
};

export const EnumInputWithExistingValues: ComponentStory<
  typeof RenderOpenApi3Form
> = () => {
  return (
    <RenderOpenApi3Form
      name="enumInput"
      getSchema={() => [
        {
          title: 'Enum Input',
          type: 'string',
          enum: ['option1', 'option2', null],
        },
        {},
      ]}
      defaultValues={{
        enumInput: 'option1',
      }}
    />
  );
};

export const Test: ComponentStory<typeof RenderOpenApi3Form> = () => (
  <RenderOpenApi3Form
    name="enumInput"
    getSchema={() => [
      {
        title: 'Enum Input',
        type: 'string',
        enum: ['option1', 'option2', null],
      },
      {},
    ]}
    defaultValues={{}}
    rawOutput
  />
);

Test.storyName = '🧪 Testing - input interaction';

Test.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await waitFor(async () => {
    await userEvent.selectOptions(canvas.getByTestId('enumInput'), 'option1');
  });

  await waitFor(async () => {
    await userEvent.click(canvas.getByTestId('submit-form-btn'));
  });

  await waitFor(async () => {
    await expect(screen.getByTestId('output').textContent).toBe(
      '{"enumInput":"option1"}'
    );
  });
};
