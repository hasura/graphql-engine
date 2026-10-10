import { StoryObj, Meta } from '@storybook/react-webpack5';
import { expect, screen, userEvent, waitFor, within } from 'storybook/test';

import { Collapsible } from './Collapsible';

export default {
  title: 'Components/Collapsible ⚛️',
  parameters: {
    docs: {
      description: {
        component: `This component is wrapping a [radix-ui collapsible component](https://www.radix-ui.com/docs/primitives/components/collapsible).<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with \`padding: 1rem;\`).`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4 w-full">{Story()}</div>],
  component: Collapsible,
} as Meta<typeof Collapsible>;

const ChildrenExample = () => (
  <div
    className="text-black dark:text-white text-center bg-gray-200 dark:bg-gray-700 py-1 rounded"
    style={{ backgroundImage: '100%' }}
  >
    &lt;--SLOT--&gt;
  </div>
);

export const ApiPlayground: StoryObj<typeof Collapsible> = {
  name: '⚙️ API',

  args: {
    triggerChildren: 'The collapse trigger children',
    children: 'The collapse trigger title',
  },
};

export const Basic: StoryObj<typeof Collapsible> = {
  render: () => (
    <Collapsible
      triggerChildren={
        <span className="font-semibold text-muted">
          The collapsible trigger
        </span>
      }
    >
      <ChildrenExample />
    </Collapsible>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      description: {
        story: `This is a basic example of a collapse with a trigger.`,
      },
      source: { state: 'open' },
    },
  },
};

export const ComponentInTrigger: StoryObj<typeof Collapsible> = {
  render: () => (
    <Collapsible triggerChildren={<ChildrenExample />}>
      <ChildrenExample />
    </Collapsible>
  ),

  name: '🧰 Component in trigger',

  parameters: {
    docs: {
      description: {
        story: `A component can be used both for trigger and content`,
      },
      source: { state: 'open' },
    },
  },
};

export const StateOpen: StoryObj<typeof Collapsible> = {
  render: () => (
    <Collapsible triggerChildren={<ChildrenExample />} defaultOpen>
      <ChildrenExample />
    </Collapsible>
  ),

  name: '🔁 State - Open',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const TestingClickedInteraction: StoryObj<typeof Collapsible> = {
  render: () => (
    <Collapsible triggerChildren={<ChildrenExample />}>
      <ChildrenExample />
    </Collapsible>
  ),

  name: '🧪 Testing - Clicked interaction',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await waitFor(async () => {
      await userEvent.click(canvas.getByTestId('collapsible-trigger'));
    });
    await waitFor(() => {
      expect(screen.getByTestId('collapsible-content')).toBeInTheDocument();
    });
    await waitFor(async () => {
      await userEvent.click(canvas.getByTestId('collapsible-trigger'));
    });
    await waitFor(() => {
      expect(screen.getByTestId('collapsible-content')).toBeEmptyDOMElement();
    });
  },

  parameters: {
    docs: {
      description: {
        story: 'Interaction test, collapsible is displayed then hidden.',
      },
      source: { state: 'open' },
    },
  },
};
