import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { screen } from '@testing-library/dom';

import { Tooltip } from '@/new-components/Tooltip';

export default {
  title: 'components/Tooltip 📁/Tooltip ⚛️',
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
  component: Tooltip,
} as ComponentMeta<typeof Tooltip>;

const ChildrenExample = () => (
  <div
    className="text-black dark:text-white text-center bg-gray-200 dark:bg-gray-700 py-1 rounded"
    style={{ backgroundImage: '100%' }}
  >
    &lt;--SLOT--&gt;
  </div>
);

const TooltipChildrenExample = () => (
  <span>The tooltip tooltipContentChildren</span>
);

export const ApiPlayground: ComponentStory<typeof Tooltip> = args => (
  <Tooltip {...args} />
);
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  children: 'The tooltip children',
  tooltipContentChildren: 'The tooltip content children',
};

export const Basic: ComponentStory<typeof Tooltip> = () => (
  <Tooltip tooltipContentChildren={<TooltipChildrenExample />}>
    <ChildrenExample />
  </Tooltip>
);
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantSide: ComponentStory<typeof Tooltip> = () => (
  <>
    <Tooltip tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
    <Tooltip side="bottom" tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
    <Tooltip side="left" tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
    <Tooltip side="top" tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
  </>
);
VariantSide.storyName = '🎭 Variant - Side';
VariantSide.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantAlign: ComponentStory<typeof Tooltip> = () => (
  <>
    <Tooltip tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
    <Tooltip align="start" tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
    <Tooltip align="center" tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
    <Tooltip align="end" tooltipContentChildren={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
  </>
);
VariantAlign.storyName = '🎭 Variant - Align';
VariantAlign.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const TestingHoveredStyle: ComponentStory<typeof Tooltip> = () => (
  <Tooltip tooltipContentChildren={<TooltipChildrenExample />} defaultOpen>
    <ChildrenExample />
  </Tooltip>
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

export const TestingHoveredInteraction: ComponentStory<typeof Tooltip> = () => (
  <Tooltip tooltipContentChildren={<TooltipChildrenExample />}>
    <ChildrenExample />
  </Tooltip>
);
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

export const TestingScalability: ComponentStory<typeof Tooltip> = () => (
  <Tooltip
    tooltipContentChildren={`Lorem ipsum dolor sit amet, consectetur adipiscing 
        elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
        Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi 
        ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit 
        in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur 
        sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt
        mollit anim id est laborum.`}
    defaultOpen
  >
    <ChildrenExample />
  </Tooltip>
);
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
