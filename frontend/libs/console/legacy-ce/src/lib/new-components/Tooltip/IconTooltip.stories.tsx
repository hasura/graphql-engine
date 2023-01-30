import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { screen } from '@testing-library/dom';

import { IconTooltip } from '@/new-components/Tooltip';

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
} as ComponentMeta<typeof IconTooltip>;

export const ApiPlayground: ComponentStory<typeof IconTooltip> = args => (
  <IconTooltip {...args} />
);
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  message: 'the tooltip message',
};

export const Basic: ComponentStory<typeof IconTooltip> = () => (
  <IconTooltip message="The tooltip message" />
);
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantSide: ComponentStory<typeof IconTooltip> = () => (
  <>
    <IconTooltip message="The tooltip message" />
    <IconTooltip side="bottom" message="The tooltip message" />
    <IconTooltip side="left" message="The tooltip message" />
    <IconTooltip side="top" message="The tooltip message" />
  </>
);
VariantSide.storyName = '🎭 Variant - Side';

export const TestingHoveredStyle: ComponentStory<typeof IconTooltip> = () => (
  <IconTooltip defaultOpen message="The tooltip message" />
);
TestingHoveredStyle.storyName = '🧪 Testing - Hovered style';
TestingHoveredStyle.parameters = {
  docs: {
    description: {
      story: 'Story with forced tooltip display for snapshot testing style',
    },
    source: { state: 'open' },
  },
};

export const TestingHoveredInteraction: ComponentStory<
  typeof IconTooltip
> = () => <IconTooltip message="The tooltip message" />;
TestingHoveredInteraction.storyName = '🧪 Testing - Hovered interaction';
TestingHoveredInteraction.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await waitFor(async () => {
    await userEvent.hover(canvas.getByTestId('tooltip-trigger'));
  });
  await waitFor(() => {
    expect(screen.getByRole('tooltip')).toBeInTheDocument();
  });
};
TestingHoveredInteraction.parameters = {
  docs: {
    description: {
      story: 'Interaction test, tootlip is displayed then hidden.',
    },
    source: { state: 'open' },
  },
};

export const TestingScalability: ComponentStory<typeof IconTooltip> = () => (
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
);
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
