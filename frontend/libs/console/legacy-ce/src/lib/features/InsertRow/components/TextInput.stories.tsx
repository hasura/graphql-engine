import { Meta, StoryFn } from '@storybook/react';
import { handlers } from '../../../mocks/metadata.mock';
import { TextInput } from './TextInput';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  title: 'Data/Insert Row/components/TextInput',
  component: TextInput,
  parameters: {
    msw: handlers(),
  },
  argTypes: {
    onChange: { action: true },
    onInput: { action: true },
  },
} as Meta<typeof TextInput>;

const Template: StoryFn<typeof TextInput> = args => (
  <div className="max-w-screen-md">
    <TextInput {...args} />
  </div>
);

export const Base = {
  render: Template,

  args: {
    name: 'id',
    disabled: false,
    placeholder: 'placeholder...',
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    // Type in text input field
    await userEvent.type(
      await canvas.findByPlaceholderText('placeholder...'),
      'John Doe'
    );

    await expect(args.onChange).toHaveBeenCalled();
    await expect(args.onInput).toHaveBeenCalled();
  },
};

export const Disabled = {
  render: Template,

  args: {
    ...Base.args,
    disabled: true,
  },
};
