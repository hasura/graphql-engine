import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { Card } from '.';

export default {
  title: 'components/Card ⚛️',
  parameters: {
    docs: {
      description: {
        component: `A component that displays a Card of a particular color.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    Story => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
  component: Card,
} as Meta<typeof Card>;

export const ApiPlayground: StoryObj<typeof Card> = {
  render: args => (
    <Card {...args}>
      <span>The Card children</span>
    </Card>
  ),

  name: '⚙️ API',
};

export const Basic: StoryObj<typeof Card> = {
  render: () => (
    <Card>
      <span>The Card children</span>
    </Card>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantMode: StoryObj<typeof Card> = {
  render: () => (
    <>
      <Card mode="default">
        <span>Default</span>
      </Card>
      <Card mode="neutral">
        <span>Neutral</span>
      </Card>
      <Card mode="positive">
        <span>Positive</span>
      </Card>
      <Card mode="error">
        <span>Error</span>
      </Card>
      <Card mode="warning">
        <span>Warning</span>
      </Card>
    </>
  ),

  name: '🎭 Variant - Mode',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateActionnable: StoryObj<typeof Card> = {
  render: () => (
    <>
      <Card mode="default" onClick={action('onClick')}>
        <span>Default</span>
      </Card>
      <Card mode="neutral" onClick={action('onClick')}>
        <span>Neutral</span>
      </Card>
      <Card mode="positive" onClick={action('onClick')}>
        <span>Positive</span>
      </Card>
      <Card mode="error" onClick={action('onClick')}>
        <span>Error</span>
      </Card>
      <Card mode="warning" onClick={action('onClick')}>
        <span>Warning</span>
      </Card>
    </>
  ),

  name: '🔁 State - Actionnable',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const StateDisabled: StoryObj<typeof Card> = {
  render: () => (
    <>
      <Card mode="default" disabled>
        <span>Default</span>
      </Card>
      <Card mode="neutral" disabled>
        <span>Neutral</span>
      </Card>
      <Card mode="positive" disabled>
        <span>Positive</span>
      </Card>
      <Card mode="error" disabled>
        <span>Error</span>
      </Card>
      <Card mode="default" onClick={action('onClick')} disabled>
        <span>Default with action</span>
      </Card>
      <Card mode="neutral" onClick={action('onClick')} disabled>
        <span>Neutral with action</span>
      </Card>
      <Card mode="positive" onClick={action('onClick')} disabled>
        <span>Positive with action</span>
      </Card>
      <Card mode="error" onClick={action('onClick')} disabled>
        <span>Error with action</span>
      </Card>
      <Card mode="warning" onClick={action('onClick')} disabled>
        <span>Warning with action</span>
      </Card>
    </>
  ),

  name: '🔁 State - Disabled',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const TestingScalability: StoryObj<typeof Card> = {
  render: () => (
    <Card>
      <div>
        Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
        tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
        commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
        velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
        occaecat cupidatat non proident, sunt in culpa qui officia deserunt
        mollit anim id est laborum.
      </div>
    </Card>
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
