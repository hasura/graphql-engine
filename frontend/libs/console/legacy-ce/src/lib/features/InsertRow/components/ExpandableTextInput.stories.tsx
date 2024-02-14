import { Meta, StoryFn } from '@storybook/react';
import { handlers } from '../../../mocks/metadata.mock';
import { ExpandableTextInput } from './ExpandableTextInput';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

export default {
  title: 'Data/Insert Row/components/ExpandableTextInput',
  component: ExpandableTextInput,
  parameters: {
    msw: handlers(),
  },
  argTypes: {
    onChange: { action: true },
    onInput: { action: true },
  },
} as Meta<typeof ExpandableTextInput>;

const Template: StoryFn<typeof ExpandableTextInput> = args => (
  <div className="max-w-screen-md">
    <ExpandableTextInput {...args} />
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

    await expect(await canvas.findByTitle('Expand')).toBeInTheDocument();

    await userEvent.click(await canvas.findByTitle('Expand'));

    await userEvent.keyboard('Enter', { delay: 50 });

    await expect(args.onChange).toHaveBeenCalled();
    await expect(args.onInput).toHaveBeenCalled();

    await userEvent.click(await canvas.findByTitle('Collapse'));
  },
};
