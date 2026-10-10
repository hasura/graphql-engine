import { StoryObj, Meta } from '@storybook/react-webpack5';
import { expect, screen, userEvent, waitFor, within } from 'storybook/test';
import { Text } from '@radix-ui/themes';
import { Tooltip } from './Tooltip';

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
    (Story) => <div className="p-16 w-full flex justify-center">{Story()}</div>,
  ],
  component: Tooltip,
} as Meta<typeof Tooltip>;

const ChildrenExample = () => (
  <Text
    className="text-black dark:text-white text-center bg-gray-200 dark:bg-gray-700 py-1 rounded"
    style={{ backgroundImage: '100%' }}
  >
    &lt;--SLOT--&gt;
  </Text>
);

const TooltipChildrenExample = () => <Text>The tooltip content</Text>;

export const Basic: StoryObj<typeof Tooltip> = {
  render: () => (
    <Tooltip content="Add to library">
      <ChildrenExample />
    </Tooltip>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantSide: StoryObj<typeof Tooltip> = {
  render: () => (
    <>
      <Tooltip content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
      <Tooltip side="bottom" content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
      <Tooltip side="left" content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
      <Tooltip side="top" content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
    </>
  ),

  name: '🎭 Variant - Side',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantAlign: StoryObj<typeof Tooltip> = {
  render: () => (
    <>
      <Tooltip content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
      <Tooltip align="start" content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
      <Tooltip align="center" content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
      <Tooltip align="end" content={<TooltipChildrenExample />}>
        <ChildrenExample />
      </Tooltip>
    </>
  ),

  name: '🎭 Variant - Align',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const TestingHoveredStyle: StoryObj<typeof Tooltip> = {
  render: () => (
    <Tooltip content={<TooltipChildrenExample />} defaultOpen>
      <ChildrenExample />
    </Tooltip>
  ),

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

export const TestingHoveredInteraction: StoryObj<typeof Tooltip> = {
  render: () => (
    <Tooltip content={<TooltipChildrenExample />}>
      <ChildrenExample />
    </Tooltip>
  ),

  name: '🧪 Testing - Hovered interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await waitFor(() => {
      userEvent.hover(canvas.getByTestId('tooltip-trigger'));
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

export const TestingScalability: StoryObj<typeof Tooltip> = {
  render: () => (
    <Tooltip
      content={`Lorem ipsum dolor sit amet, consectetur adipiscing 
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
  ),

  name: '🧪 Testing - Scalability',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
