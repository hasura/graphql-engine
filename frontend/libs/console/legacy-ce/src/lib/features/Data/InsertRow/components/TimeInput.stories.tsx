import { Meta, StoryFn } from '@storybook/react-webpack5';
import { expect, userEvent, within } from 'storybook/test';
import { TimeInput } from './TimeInput';
import { useRef } from 'react';

export default {
  title: 'Data/Insert Row/components/TimeInput',
  component: TimeInput,
  parameters: {
    // msw: handlers(),
    mockdate: new Date('2020-01-14T15:47:18.502Z'),
  },
  argTypes: {
    onChange: { action: true },
    onInput: { action: true },
  },
} as Meta<typeof TimeInput>;

const Template: StoryFn<typeof TimeInput> = (args) => {
  const inputRef = useRef<HTMLInputElement | null>(null);
  return <TimeInput {...args} ref={inputRef} />;
};

export const Base = {
  render: Template,

  args: {
    name: 'date',
    placeholder: 'date...',
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.type(
      await canvas.findByPlaceholderText('date...'),
      '15:30:00',
    );

    await expect(args.onChange).toHaveBeenCalled();
    await expect(args.onInput).toHaveBeenCalled();

    await expect(
      await canvas.findByDisplayValue('15:30:00'),
    ).toBeInTheDocument();

    await userEvent.click(await canvas.findByRole('button'));

    await userEvent.click(await canvas.findByText('4:30 PM'));

    await expect(await canvas.findByLabelText('date')).toHaveDisplayValue(
      '16:30:00',
    );

    await expect(args.onChange).toHaveBeenCalled();
    await expect(args.onInput).toHaveBeenCalled();

    // the picker is automatically hidden on selection
    await expect(await canvas.queryByText('Time')).not.toBeInTheDocument();
  },
};

export const Disabled = {
  render: Template,

  args: {
    ...Base.args,
    disabled: true,
  },
};
