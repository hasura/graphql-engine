import { Meta, StoryObj } from '@storybook/react';
import { Sidebar } from './Sidebar';
import { useState } from 'react';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
export default {
  component: Sidebar,
  parameters: {
    layout: 'fullscreen',
  },
  decorators: [ReactQueryDecorator()],
} satisfies Meta<typeof Sidebar>;

export const Playground: StoryObj<typeof Sidebar> = {
  render: args => {
    const [activeLink, setActiveLink] = useState('SQL');
    return (
      <div style={{ width: 400, height: '100vh' }}>
        <Sidebar
          isLinkActive={link => link.name === activeLink}
          onLinkClick={link => setActiveLink(link.name)}
        />
      </div>
    );
  },
};
