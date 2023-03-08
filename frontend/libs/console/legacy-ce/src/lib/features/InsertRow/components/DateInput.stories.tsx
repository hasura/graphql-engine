import { ComponentMeta, ComponentStory } from '@storybook/react';
import { handlers } from '../../../mocks/metadata.mock';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { DateInput } from './DateInput';
import { useRef } from 'react';
import { format } from 'date-fns';

export default {
  title: 'Data/Insert Row/components/DateInput',
  component: DateInput,
  parameters: {
    msw: handlers(),
    mockdate: new Date('2020-01-14T15:47:18.502Z'),
  },
  argTypes: {
    onChange: { action: true },
    onInput: { action: true },
  },
} as ComponentMeta<typeof DateInput>;

const Template: ComponentStory<typeof DateInput> = args => {
  const inputRef = useRef<HTMLInputElement | null>(null);
  return <DateInput {...args} inputRef={inputRef} />;
};

export const Base = Template.bind({});
Base.args = {
  name: 'date',
  placeholder: 'date...',
};

export const Disabled = Template.bind({});
Disabled.args = {
  ...Base.args,
  disabled: true,
};

Base.play = async ({ args, canvasElement }) => {
  const canvas = within(canvasElement);

  userEvent.type(await canvas.findByPlaceholderText('date...'), '2020-01-14');

  expect(args.onChange).toHaveBeenCalled();
  expect(args.onInput).toHaveBeenCalled();

  const baseDate = new Date();

  expect(await canvas.findByDisplayValue('2020-01-14')).toBeInTheDocument();

  userEvent.click(await canvas.findByRole('button'));

  const baseDateLabel = `Choose ${format(baseDate, 'EEEE, LLLL do, u')}`;

  userEvent.click((await canvas.findAllByLabelText(baseDateLabel))[0]);

  const baseDateValue = format(baseDate, 'yyyy-LL-dd');
  expect(await canvas.findByLabelText('date')).toHaveDisplayValue(
    baseDateValue
  );

  expect(args.onChange).toHaveBeenCalled();
  expect(args.onInput).toHaveBeenCalled();
};
