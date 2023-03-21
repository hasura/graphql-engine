import { ComponentMeta, ComponentStory } from '@storybook/react';
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
} as ComponentMeta<typeof TextInput>;

const Template: ComponentStory<typeof TextInput> = args => (
  <div className="max-w-screen-md">
    <TextInput {...args} />
  </div>
);

export const Base = Template.bind({});
Base.args = {
  name: 'id',
  disabled: false,
  placeholder: 'placeholder...',
};

export const Disabled = Template.bind({});
Disabled.args = {
  ...Base.args,
  disabled: true,
};

Base.play = async ({ args, canvasElement }) => {
  const canvas = within(canvasElement);

  // Type in text input field
  await userEvent.type(
    await canvas.findByPlaceholderText('placeholder...'),
    'John Doe'
  );

  await expect(args.onChange).toHaveBeenCalled();
  await expect(args.onInput).toHaveBeenCalled();
};
