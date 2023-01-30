import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { screen } from '@testing-library/dom';

import { Collapsible } from '@/new-components/Collapsible';

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
  decorators: [Story => <div className="p-4 w-full">{Story()}</div>],
  component: Collapsible,
} as ComponentMeta<typeof Collapsible>;

const ChildrenExample = () => (
  <div
    className="text-black dark:text-white text-center bg-gray-200 dark:bg-gray-700 py-1 rounded"
    style={{ backgroundImage: '100%' }}
  >
    &lt;--SLOT--&gt;
  </div>
);

export const ApiPlayground: ComponentStory<typeof Collapsible> = args => (
  <Collapsible {...args} />
);
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  triggerChildren: 'The collapse trigger children',
  children: 'The collapse trigger title',
};

export const Basic: ComponentStory<typeof Collapsible> = () => (
  <Collapsible
    triggerChildren={
      <span className="font-semibold text-muted">The collapsible trigger</span>
    }
  >
    <ChildrenExample />
  </Collapsible>
);
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    description: {
      story: `This is a basic example of a collapse with a trigger.`,
    },
    source: { state: 'open' },
  },
};

export const ComponentInTrigger: ComponentStory<typeof Collapsible> = () => (
  <Collapsible triggerChildren={<ChildrenExample />}>
    <ChildrenExample />
  </Collapsible>
);
ComponentInTrigger.storyName = '🧰 Component in trigger';
ComponentInTrigger.parameters = {
  docs: {
    description: {
      story: `A component can be used both for trigger and content`,
    },
    source: { state: 'open' },
  },
};

export const StateOpen: ComponentStory<typeof Collapsible> = () => (
  <Collapsible triggerChildren={<ChildrenExample />} defaultOpen>
    <ChildrenExample />
  </Collapsible>
);
StateOpen.storyName = '🔁 State - Open';
StateOpen.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const TestingClickedInteraction: ComponentStory<
  typeof Collapsible
> = () => (
  <Collapsible triggerChildren={<ChildrenExample />}>
    <ChildrenExample />
  </Collapsible>
);
TestingClickedInteraction.storyName = '🧪 Testing - Clicked interaction';
TestingClickedInteraction.play = async ({ canvasElement }) => {
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
};
TestingClickedInteraction.parameters = {
  docs: {
    description: {
      story: 'Interaction test, collapsible is displayed then hidden.',
    },
    source: { state: 'open' },
  },
};
