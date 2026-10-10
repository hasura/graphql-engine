import { Meta, StoryFn } from '@storybook/react-webpack5';
import { expect, fn, userEvent, within } from 'storybook/test';
import { DateInput } from './DateInput';
import { useRef } from 'react';
import { format } from 'date-fns';

export default {
  title: 'Data/Insert Row/components/DateInput',
  component: DateInput,
  parameters: {
    mockdate: new Date('2020-01-14T15:47:18.502Z'),
  },
  // `fn()` spies (not `argTypes.action`) so the play's
  // `expect(args.onChange).toHaveBeenCalled()` has a real spy to assert on;
  // Storybook still logs fn() spies in the Actions panel.
  args: {
    onChange: fn(),
    onInput: fn(),
  },
} as Meta<typeof DateInput>;

const Template: StoryFn<typeof DateInput> = (args) => {
  const inputRef = useRef<HTMLInputElement | null>(null);
  return <DateInput {...args} ref={inputRef} />;
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
      '2020-01-14',
    );

    await expect(args.onChange).toHaveBeenCalled();
    await expect(args.onInput).toHaveBeenCalled();

    const baseDate = new Date();

    await expect(
      await canvas.findByDisplayValue('2020-01-14'),
    ).toBeInTheDocument();

    await userEvent.click(await canvas.findByRole('button'));

    const baseDateLabel = `Choose ${format(baseDate, 'EEEE, LLLL do, u')}`;

    await userEvent.click((await canvas.findAllByLabelText(baseDateLabel))[0]);

    const baseDateValue = format(baseDate, 'yyyy-LL-dd');
    await expect(await canvas.findByLabelText('date')).toHaveDisplayValue(
      baseDateValue,
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
