import React from 'react';
import { StoryObj, Meta } from '@storybook/react';

import { NavigationSidebar } from '.';

export default {
  title: 'components/NavigationSidebar ⚛️',
  parameters: {
    docs: {
      description: {
        component: `A component that displays a badge of a particular color.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    Story => (
      <div className="p-4 flex gap-5 items-center max-w-xs">{Story()}</div>
    ),
  ],
  component: NavigationSidebar,
} as Meta<typeof NavigationSidebar>;

export const ApiPlayground: StoryObj<typeof NavigationSidebar> = {
  name: '⚙️ API',

  args: {
    location: {
      action: 'POP',
      hash: '',
      key: '5nvxpbdafa',
      pathname: 'route-2',
      search: '',
      state: undefined,
      query: {},
    },
    sections: [
      {
        key: 'section-1',
        label: 'Section 1',
        items: [
          {
            key: '1-1',
            label: 'Item 1-1',
            status: 'enabled',
            route: 'route-1',
          },
          {
            key: '1-2',
            label: 'Item 1-2',
            status: 'disabled',
            route: 'route-2',
          },
          {
            key: '1-3',
            label: 'Item 1-3',
            status: 'none',
            route: 'route-3',
          },
          {
            key: '1-4',
            label: 'Item 1-3',
            status: 'error',
            route: 'route-3',
          },
        ],
      },
      {
        key: 'section-2',
        label: 'Section 2',
        items: [
          {
            key: '2-1',
            label: 'Item 2-1',
            status: 'enabled',
            route: 'route-1',
          },
          {
            key: '2-2',
            label: 'Item 2-2',
            status: 'disabled',
            route: 'route-2',
          },
          {
            key: '2-3',
            label: 'Item 2-3',
            status: 'none',
            route: 'route-3',
          },
          {
            key: '2-4',
            label: 'Item 2-3',
            status: 'error',
            route: 'route-3',
          },
        ],
      },
    ],
  },
};
