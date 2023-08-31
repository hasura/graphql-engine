import { Meta, StoryObj } from '@storybook/react';
import { ReactQueryDecorator } from '../../storybook/decorators/react-query';
import { SidebarUI } from './Sidebar';
import { handlers } from './NavTree/mocks/handlers';
import { mockMetadata } from './NavTree/mocks/mockData';
import { useState } from 'react';
export default {
  component: SidebarUI,
  parameters: {
    layout: 'fullscreen',
    msw: handlers(),
  },
  decorators: [ReactQueryDecorator()],
} satisfies Meta<typeof SidebarUI>;

export const Playground: StoryObj<typeof SidebarUI> = {
  args: {
    metadataError: null,
    sources: mockMetadata.metadata.sources,
    onRetryMetadata: () => {},
    metadataQueryStatus: 'success',
  },
  render: args => {
    const [refetching, setRefetching] = useState(false);
    return (
      <div style={{ width: 400, height: '100vh' }}>
        <SidebarUI
          {...args}
          isRefetching={refetching}
          onRetryMetadata={() => {
            setRefetching(true);
            setTimeout(() => {
              setRefetching(false);
            }, 2000);
          }}
        />
      </div>
    );
  },
};
