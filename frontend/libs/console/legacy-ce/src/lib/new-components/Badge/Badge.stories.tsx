import React from 'react';
import { StoryObj, Meta } from '@storybook/react';

import { Badge } from '.';

export default {
  title: 'components/Badge ⚛️',
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
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
  component: Badge,
} as Meta<typeof Badge>;

export const ApiPlayground: StoryObj<typeof Badge> = {
  render: args => (
    <Badge {...args}>
      <span>The Badge children</span>
    </Badge>
  ),

  name: '⚙️ API',
};

export const Basic: StoryObj<typeof Badge> = {
  render: () => (
    <Badge>
      <span>The Badge children</span>
    </Badge>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantColor: StoryObj<typeof Badge> = {
  render: () => (
    <>
      <Badge color="green">
        <span>Green</span>
      </Badge>
      <Badge color="red">
        <span>Red</span>
      </Badge>
      <Badge color="indigo">
        <span>Indigo</span>
      </Badge>
      <Badge color="gray">
        <span>Gray</span>
      </Badge>
      <Badge color="yellow">
        <span>Yellow</span>
      </Badge>
    </>
  ),

  name: '🎭 Variant - Mode',

  parameters: {
    docs: {
      description: {
        story: `#### 🚦 Usage
  - The default Badge should be used in the vast majority of circumstances
  - The primary Badge style should only be used once per page for create / persistent actions, we also shouldn't have
    any other sizes for the primary Badge
  - Use destructive variant for destructive actions only`,
      },
      source: { state: 'open' },
    },
  },
};

export const TestingScalability: StoryObj<typeof Badge> = {
  render: () => (
    <Badge>
      <div className="max-w-full text-ellipsis overflow-hidden">
        Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
        tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
        commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
        velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
        occaecat cupidatat non proident, sunt in culpa qui officia deserunt
        mollit anim id est laborum.
      </div>
    </Badge>
  ),

  name: '🧪 Testing - Scalability',

  parameters: {
    docs: {
      description: {
        story: `⚠️ Please add some defensive checks in the component children to prevent them to overflow.`,
      },
      source: { state: 'open' },
    },
  },
};
