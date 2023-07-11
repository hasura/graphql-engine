import { Meta, StoryObj } from '@storybook/react';
import { Tabs } from './Tabs';
import randomWords from 'random-words';
import { useState } from 'react';
export default {
  component: Tabs,
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof Tabs>;

const Content: React.FC = ({ children }) => (
  <div data-chromatic="ignore" className="whitespace-pre-line break-words p-2">
    {children}
  </div>
);

export const Playground: StoryObj<typeof Tabs> = {
  args: {
    items: [
      {
        value: 'tab-1',
        label: 'Tab 1',
        content: <Content>{randomWords(100).join(' ')}</Content>,
      },
      {
        value: 'tab-2',
        label: 'Tab 2',
        content: <Content>{randomWords(100).join(' ')}</Content>,
      },
      {
        value: 'tab-3',
        label: 'Tab 3',
        content: <Content>{randomWords(100).join(' ')}</Content>,
      },
    ],
  },
  render: args => (
    <div className="m-2 max-w-xl">
      <Tabs {...args} />
    </div>
  ),
};

export const Controlled: StoryObj<typeof Tabs> = {
  args: { ...Playground.args },
  render: args => {
    const [tab, setTab] = useState('tab-1');
    return (
      <div className="m-2 max-w-xl">
        <Tabs {...args} value={tab} onValueChange={setTab} />
      </div>
    );
  },
};
