import { Meta, StoryFn } from '@storybook/react';
import { handlers } from '../../../mocks/metadata.mock';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { TimeInput } from './TimeInput';
import { useRef } from 'react';

export default {
  title: 'Data/Insert Row/components/TimeInput',
  component: TimeInput,
  parameters: {
    msw: handlers(),
    mockdate: new Date('2020-01-14T15:47:18.502Z'),
  },
  argTypes: {
    onChange: { action: true },
    onInput: { action: true },
  },
} as Meta<typeof TimeInput>;

const Template: StoryFn<typeof TimeInput> = args => {
  const inputRef = useRef<HTMLInputElement | null>(null);
  return <TimeInput {...args} inputRef={inputRef} />;
};

export const Base = {
  render: Template,

  args: {
    name: 'date',
    placeholder: 'date...',
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    userEvent.type(await canvas.findByPlaceholderText('date...'), '15:30:00');

    expect(args.onChange).toHaveBeenCalled();
    expect(args.onInput).toHaveBeenCalled();

    expect(await canvas.findByDisplayValue('15:30:00')).toBeInTheDocument();

    userEvent.click(await canvas.findByRole('button'));

    userEvent.click(await canvas.findByText('4:30 PM'));

    expect(await canvas.findByLabelText('date')).toHaveDisplayValue('16:30:00');

    expect(args.onChange).toHaveBeenCalled();
    expect(args.onInput).toHaveBeenCalled();

    // the picker is automatically hidden on selection
    expect(await canvas.queryByText('Time')).not.toBeInTheDocument();
  },
};

export const Disabled = {
  render: Template,

  args: {
    ...Base.args,
    disabled: true,
  },
};
