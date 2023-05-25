import { Meta, StoryFn } from '@storybook/react';
import { handlers } from '../../../mocks/metadata.mock';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { DateTimeInput } from './DateTimeInput';
import { useRef } from 'react';
import { format } from 'date-fns';

export default {
  title: 'Data/Insert Row/components/DateTimeInput',
  component: DateTimeInput,
  parameters: {
    msw: handlers(),
    mockdate: new Date('2020-01-14T15:47:18.502Z'),
  },
  argTypes: {
    onChange: { action: true },
    onInput: { action: true },
  },
} as Meta<typeof DateTimeInput>;

const Template: StoryFn<typeof DateTimeInput> = args => {
  const inputRef = useRef<HTMLInputElement | null>(null);
  return <DateTimeInput {...args} inputRef={inputRef} />;
};

export const Base = {
  render: Template,

  args: {
    name: 'date',
    placeholder: 'date...',
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);

    userEvent.type(
      await canvas.findByPlaceholderText('date...'),
      '2020-01-14T12:00:00.000Z'
    );

    expect(args.onChange).toHaveBeenCalled();
    expect(args.onInput).toHaveBeenCalled();

    const baseDate = new Date();

    expect(
      await canvas.findByDisplayValue('2020-01-14T12:00:00.000Z')
    ).toBeInTheDocument();

    userEvent.click(await canvas.findByRole('button'));

    const baseDateLabel = `Choose ${format(baseDate, 'EEEE, LLLL do, u')}`;

    userEvent.click((await canvas.findAllByLabelText(baseDateLabel))[0]);
    userEvent.click(await canvas.findByText('4:30 PM'));

    expect(await canvas.findByLabelText('date')).toHaveDisplayValue(
      '2020-01-14T16:30:00.502Z'
    );

    expect(args.onChange).toHaveBeenCalled();
    expect(args.onInput).toHaveBeenCalled();

    // click outside the calendar picker
    userEvent.click(await canvas.findByLabelText('date'));

    expect(await canvas.queryByText('January 2020')).not.toBeInTheDocument();
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
