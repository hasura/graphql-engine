import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { screen } from '@storybook/testing-library';

import { IconTooltip } from '.';

export default {
  title: 'components/Tooltip 📁/IconTooltip 🧬',
  parameters: {
    docs: {
      description: {
        component: `This component is wrapping a [radix-ui tooltip component](https://www.radix-ui.com/docs/primitives/components/tooltip)
triggered when one of its children is hovered. The side of the tooltip is determined by the \`side\` prop.<br>
Default CSS display is \`inline\`, provided without padding and margin (displayed here with \`padding: 2rem;\`).`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    Story => <div className="p-16 w-full flex justify-center">{Story()}</div>,
  ],
  component: IconTooltip,
} as Meta<typeof IconTooltip>;

export const ApiPlayground: StoryObj<typeof IconTooltip> = {
  name: '⚙️ API',

  args: {
    message: 'the tooltip message',
  },
};

export const Basic: StoryObj<typeof IconTooltip> = {
  render: () => <IconTooltip message="The tooltip message" />,

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantSide: StoryObj<typeof IconTooltip> = {
  render: () => (
    <>
      <IconTooltip message="The tooltip message" />
      <IconTooltip side="bottom" message="The tooltip message" />
      <IconTooltip side="left" message="The tooltip message" />
      <IconTooltip side="top" message="The tooltip message" />
    </>
  ),

  name: '🎭 Variant - Side',
};

export const TestingHoveredStyle: StoryObj<typeof IconTooltip> = {
  render: () => <IconTooltip defaultOpen message="The tooltip message" />,

  name: '🧪 Testing - Hovered style',

  parameters: {
    docs: {
      description: {
        story: 'Story with forced tooltip display for snapshot testing style',
      },
      source: { state: 'open' },
    },
  },
};

export const TestingHoveredInteraction: StoryObj<typeof IconTooltip> = {
  render: () => <IconTooltip message="The tooltip message" />,
  name: '🧪 Testing - Hovered interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await waitFor(async () => {
      await userEvent.hover(canvas.getByTestId('tooltip-trigger'));
    });
    await waitFor(() => {
      expect(screen.getByRole('tooltip')).toBeInTheDocument();
    });
  },

  parameters: {
    docs: {
      description: {
        story: 'Interaction test, tootlip is displayed then hidden.',
      },
      source: { state: 'open' },
    },
  },
};

export const TestingScalability: StoryObj<typeof IconTooltip> = {
  render: () => (
    <IconTooltip
      message={`Lorem ipsum dolor sit amet, consectetur adipiscing 
          elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
          Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi 
          ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit 
          in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur 
          sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt
          mollit anim id est laborum.`}
      defaultOpen
    />
  ),

  name: '🧪 Testing - Scalability',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
