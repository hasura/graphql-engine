import { Meta, StoryObj } from '@storybook/react';
import { useState } from 'react';
import { Tabs } from './Tabs';
export default {
  component: Tabs,
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof Tabs>;

const Content: React.FC = ({ children }) => (
  <div className="whitespace-pre-line break-words p-2">{children}</div>
);

export const Playground: StoryObj<typeof Tabs> = {
  args: {
    items: [
      {
        value: 'tab-1',
        label: 'Tab 1',
        content: (
          <Content>
            On it differed repeated wandered required in. Then girl neat why yet
            knew rose spot. Moreover property we he kindness greatest be oh
            striking laughter. In me he at collecting affronting principles
            apartments. Has visitor law attacks pretend you calling own excited
            painted. Contented attending smallness it oh ye unwilling. Turned
            favour man two but lovers. Suffer should if waited common person
            little oh. Improved civility graceful sex few smallest screened
            settling. Likely active her warmly has.
          </Content>
        ),
      },
      {
        value: 'tab-2',
        label: 'Tab 2',
        content: (
          <Content>
            As absolute is by amounted repeated entirely ye returned. These
            ready timed enjoy might sir yet one since. Years drift never if
            could forty being no. On estimable dependent as suffering on my.
            Rank it long have sure in room what as he. Possession travelling
            sufficient yet our. Talked vanity looked in to. Gay perceive led
            believed endeavor. Rapturous no of estimable oh therefore direction
            up. Sons the ever not fine like eyes all sure.
          </Content>
        ),
      },
      {
        value: 'tab-3',
        label: 'Tab 3',
        content: (
          <Content>
            New had happen unable uneasy. Drawings can followed improved out
            sociable not. Earnestly so do instantly pretended. See general few
            civilly amiable pleased account carried. Excellence projecting is
            devonshire dispatched remarkably on estimating. Side in so life
            past. Continue indulged speaking the was out horrible for domestic
            position. Seeing rather her you not esteem men settle genius excuse.
            Deal say over you age from. Comparison new ham melancholy son
            themselves.
          </Content>
        ),
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
