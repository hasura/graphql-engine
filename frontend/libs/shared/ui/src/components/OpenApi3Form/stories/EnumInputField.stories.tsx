import { StoryObj, StoryFn, Meta } from '@storybook/react-webpack5';
import { expect, screen, userEvent, waitFor, within } from 'storybook/test';
import { RenderOpenApi3Form } from './common/RenderOpenApi3Form';
import { ReactQueryDecorator } from '@hasura/shared/testing';

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
    (Story) => <div className="p-4 w-full">{Story()}</div>,
  ],
} as Meta<typeof RenderOpenApi3Form>;

export const EnumInput: StoryFn<typeof RenderOpenApi3Form> = () => {
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

export const EnumInputWithNullableAsOption: StoryFn<
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

export const EnumInputWithExistingValues: StoryFn<
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

export const Test: StoryObj<typeof RenderOpenApi3Form> = {
  render: () => (
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
  ),

  name: '🧪 Testing - input interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await waitFor(async () => {
      await userEvent.selectOptions(canvas.getByTestId('enumInput'), 'option1');
    });

    await waitFor(async () => {
      await userEvent.click(canvas.getByTestId('submit-form-btn'));
    });

    await waitFor(async () => {
      await expect(screen.getByTestId('output').textContent).toBe(
        '{"enumInput":"option1"}',
      );
    });
  },
};
