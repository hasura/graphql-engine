import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';

import { Badge } from '@/new-components/Badge';

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
} as ComponentMeta<typeof Badge>;

export const ApiPlayground: ComponentStory<typeof Badge> = args => (
  <Badge {...args}>
    <span>The Badge children</span>
  </Badge>
);
ApiPlayground.storyName = '⚙️ API';

export const Basic: ComponentStory<typeof Badge> = () => (
  <Badge>
    <span>The Badge children</span>
  </Badge>
);
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantColor: ComponentStory<typeof Badge> = () => (
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
);
VariantColor.storyName = '🎭 Variant - Mode';
VariantColor.parameters = {
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
};

export const TestingScalability: ComponentStory<typeof Badge> = () => (
  <Badge>
    <div className="max-w-full text-ellipsis overflow-hidden">
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
      veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
      commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
      velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
      cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
      est laborum.
    </div>
  </Badge>
);
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    description: {
      story: `⚠️ Please add some defensive checks in the component children to prevent them to overflow.`,
    },
    source: { state: 'open' },
  },
};
