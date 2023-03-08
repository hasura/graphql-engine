import { ComponentMeta, ComponentStory } from '@storybook/react';
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
} as ComponentMeta<typeof ExpandableTextInput>;

const Template: ComponentStory<typeof ExpandableTextInput> = args => (
  <div className="max-w-screen-md">
    <ExpandableTextInput {...args} />
  </div>
);

export const Base = Template.bind({});
Base.args = {
  name: 'id',
  disabled: false,
  placeholder: 'placeholder...',
};

Base.play = async ({ args, canvasElement }) => {
  const canvas = within(canvasElement);

  // Type in text input field
  userEvent.type(
    await canvas.findByPlaceholderText('placeholder...'),
    'John Doe'
  );

  expect(args.onChange).toHaveBeenCalled();
  expect(args.onInput).toHaveBeenCalled();

  expect(await canvas.findByTitle('Expand')).toBeInTheDocument();

  await userEvent.click(await canvas.findByTitle('Expand'));

  await userEvent.keyboard('Enter', { delay: 50 });

  expect(args.onChange).toHaveBeenCalled();
  expect(args.onInput).toHaveBeenCalled();

  await userEvent.click(await canvas.findByTitle('Collapse'));
};
