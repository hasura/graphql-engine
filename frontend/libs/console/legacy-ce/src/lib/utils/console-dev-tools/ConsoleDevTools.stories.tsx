import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ConsoleDevToolsInner } from './ConsoleDevTools';

export default {
  component: ConsoleDevToolsInner,
  title: 'ConsoleDevTools',
} as ComponentMeta<typeof ConsoleDevToolsInner>;

const Template: ComponentStory<typeof ConsoleDevToolsInner> = args => {
  return <ConsoleDevToolsInner {...args} />;
};

export const Primary = Template.bind({});
Primary.args = {};
