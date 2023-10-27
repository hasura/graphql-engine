import type { Meta, StoryObj } from '@storybook/react';
import { NavTreeUI } from './NavTree';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { mockTreeData } from './mocks/treeData';
import { SidebarContext } from '../SidebarContext';
import { useState } from 'react';

export default {
  component: NavTreeUI,
  decorators: [
    ReactQueryDecorator(),
    story => (
      <SidebarContext.Provider value={{ loadingItems: [], loadingSources: [] }}>
        {story()}
      </SidebarContext.Provider>
    ),
  ],
} satisfies Meta<typeof NavTreeUI>;

type Story = StoryObj<typeof NavTreeUI>;

export const Primary: Story = {
  render: () => {
    const [status, setStatus] = useState('');
    return (
      <div>
        <div>{status}</div>
        <div className="max-w-lg border border-slate-600 rounded-sm bg-white ">
          <NavTreeUI
            treeData={mockTreeData}
            handleDatabaseClick={source => {
              setStatus(`Clicked Database: ${source}`);
            }}
            handleDatabaseObjectClick={({ dataSourceName, ...details }) => {
              setStatus(
                `Clicked ${dataSourceName} item:\n ${JSON.stringify(details)}`
              );
            }}
          />
        </div>
      </div>
    );
  },
};
