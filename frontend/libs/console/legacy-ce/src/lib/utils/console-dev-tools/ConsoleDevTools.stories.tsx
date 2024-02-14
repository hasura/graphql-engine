import { StoryFn, Meta } from '@storybook/react';
import { ConsoleDevToolsInner } from './ConsoleDevTools';

export default {
  component: ConsoleDevToolsInner,
  title: 'ConsoleDevTools',
} as Meta<typeof ConsoleDevToolsInner>;

const Template: StoryFn<typeof ConsoleDevToolsInner> = args => {
  return <ConsoleDevToolsInner {...args} />;
};

export const Primary = {
  render: Template,
  args: {},
};
